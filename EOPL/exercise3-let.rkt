#lang eopl
(require rackunit)

(define zip
  (lambda (xs ys)
    (if (or (null? xs) (null? ys))
        '()
        (cons
         (cons (car xs) (car ys))
         (zip (cdr xs) (cdr ys))))))
(define fold
  (lambda (z append xs)
    (if (null? xs)
        z
        (fold (append z (car xs)) append (cdr xs)))))

(define-datatype env env?
  (empty-env)
  (extend-env
   (var symbol?)
   (val expval?)
   (saved-env env?)))
(define apply-env
  (lambda (search-var given-env)
    (cases env given-env
           (extend-env (var val saved-env)
                       (if (eqv? var search-var)
                           val
                           (apply-env search-var saved-env)))
           (else eopl:error "no binding for free variable"))))

(define-datatype expval expval?
  (num-val
   (num number?))
  (list-val
   (list list?)))
(define bool-val
  (lambda (p)
    (num-val (if p 1 0))))
(define val->expval
  (lambda (val)
    (cond
     ((number? val) (num-val val))
     ((list? val) (list-val val))
     (else (eopl:error val)))))
(define expval->val
  (lambda (val)
    (cases expval val
           (num-val (num) num)
           (list-val (list) list)
           (else (eopl:error val)))))
(define expval->num
  (lambda (val)
    (cases expval val
           (num-val (num) num)
           (else (eopl:error 'num val)))))
(define expval->bool
  (lambda (val)
    (cases expval val
           (num-val (num)
                    (cond
                     ((= 0 num) #f)
                     ((= 1 num) #t)
                     (else (eopl:error "invalid bool"))))
           (else (eopl:error "bool" val)))))
(define expval->list
  (lambda (val)
    (cases expval val
           (list-val (list) list)
           (else (eopl:error "list" val)))))

(define arith-scanner
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))
(define arith-grammer
  '((expression
    (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")") diff-exp)
    (expression
     ("+" "(" expression "," expression ")") plus-exp)
    (expression
     ("*" "(" expression "," expression ")") mul-exp)
    (expression
     ("/" "(" expression "," expression ")") div-exp)
    (expression
     ("minus" "(" expression ")") minus-exp)
    (expression
     ("emptylist") emptylist-exp)
    (expression
     ("cons" "(" expression "," expression ")") cons-exp)
    (expression
     ("car" "(" expression ")") car-exp)
    (expression
     ("cdr" "(" expression ")") cdr-exp)
    (expression
     ("list" "(" (separated-list expression ",") ")") list-exp)
    (expression
     (identifier) var-exp)
    (expression
     ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression
     ("let*" (arbno identifier "=" expression) "in" expression) let*-exp)
    (expression
     ("unpack" (arbno identifier) "=" expression "in" expression) unpack-exp)
    (expression
     ("if" bool-exp "then" expression "else" expression) if-exp)
    (expression
     ("cond" (arbno bool-exp "==>" expression) "end") cond-exp)
    (expression
     ("print" "(" expression ")") print-exp)
    (bool-exp
     ("zero?" "(" expression ")") zero?-exp)
    (bool-exp
     ("equal?" "(" expression "," expression ")") equal?-exp)
    (bool-exp
     ("greater?" "(" expression "," expression ")") greater?-exp)
    (bool-exp
     ("less?" "(" expression "," expression ")") less?-exp)
    (bool-exp
     ("null?" "(" expression ")") null?-exp)))
(sllgen:make-define-datatypes arith-scanner arith-grammer)

(define eval-let-exp
  (lambda (vars val-exps body env)
    (let ((vals (map (lambda (expr) (value-of expr env)) val-exps)))
      (let ((new-env (fold env
                           (lambda (extended-env pair)
                             (extend-env (car pair) (cdr pair) extended-env))
                           (zip vars vals))))
        (value-of body new-env)))))
(define eval-let*-exp
  (lambda (vars val-exps body env)
    (let ((new-env (fold env
                         (lambda (extended-env pair)
                           (let ((var (car pair))
                                 (val (value-of (cdr pair) extended-env)))
                             (extend-env var val extended-env)))
                         (zip vars val-exps))))
      (value-of body new-env))))
(define eval-if-exp
  (lambda (predicate if-exp false-exp env)
    (let ((pred (value-of->bool predicate env)))
      (if pred
          (value-of if-exp env)
          (value-of false-exp env)))))
(define eval-cons-exp
  (lambda (expr1 expr2 env)
    ((lift list-val expval->val) cons env expr1 expr2)))
(define eval-cond-exp
  (lambda (predicates exprs env)
    (if (or (null? predicates) (null? exprs))
        (eopl:error "nont of predictor meets")
        (let ((predicate (car predicates))
              (expr (car exprs)))
          (if (value-of->bool predicate env)
              (val->expval (value-of->val expr env))
              (eval-cond-exp (cdr predicates) (cdr exprs) env))))))
(define eval-unpack-exp
  (lambda (vars lst-vals body env)
    (let ((vals (value-of->list lst-vals env)))
      (if (= (length vars) (length vals))
          (let ((new-env (fold env
                               (lambda (extended-env pair)
                                 (let ((var (car pair))
                                       (val (cdr pair)))
                                   (extend-env var (val->expval val) extended-env)))
                               (zip vars vals))))
            (value-of body new-env))
          (eopl:error "identifiers and list cannot match")))))

(define value-of-expression
  (lambda (exp env)
    (cases expression exp
           (const-exp (num) (num-val num))
           (var-exp (var) (apply-env var env))
           (diff-exp (expr1 expr2) ((lift num-val expval->num) - env  expr1 expr2))
           (plus-exp (expr1 expr2) ((lift num-val expval->num) + env  expr1 expr2))
           (div-exp (expr1 expr2) ((lift num-val expval->num) / env  expr1 expr2))
           (mul-exp (expr1 expr2) ((lift num-val expval->num) * env  expr1 expr2))
           (minus-exp (expr) ((lift num-val expval->num) - env  expr))
           (emptylist-exp () (list-val '()))
           (cons-exp (expr1 expr2) (eval-cons-exp expr1 expr2 env))
           (car-exp (expr) (val->expval (car (value-of->list expr env))))
           (cdr-exp (expr) (val->expval (cdr (value-of->list expr env))))
           (list-exp (exprs) (list-val (map (lambda (expr) (value-of->val expr env)) exprs)))
           (if-exp (predicate if-exp false-exp) (eval-if-exp predicate if-exp false-exp env))
           (cond-exp (predicates exprs) (eval-cond-exp predicates exprs env))
           (let-exp (vars val-exps body) (eval-let-exp vars val-exps body env))
           (let*-exp (vars val-exps body) (eval-let*-exp vars val-exps body env))
           (unpack-exp (idens lst-exp body) (eval-unpack-exp idens lst-exp body env))
           (print-exp (exp)
                      (display (value-of->val exp env))
                      (num-val 1)))))
(define value-of-bool-exp
  (lambda (exp env)
    (cases bool-exp exp
           (equal?-exp (expr1 expr2) ((lift bool-val expval->num) = env expr1 expr2))
           (greater?-exp (expr1 expr2) ((lift bool-val expval->num) > env expr1 expr2))
           (less?-exp (expr1 expr2) ((lift bool-val expval->num) < env  expr1 expr2))
           (zero?-exp (expr) ((lift bool-val expval->num) zero? env expr))
           (null?-exp (expr) (bool-val (null? (value-of->list expr env)))))))
(define value-of
  (lambda (exp env)
    (cond
     ((expression? exp) (value-of-expression exp env))
     ((bool-exp? exp) (value-of-bool-exp exp env))
     (else (eopl:error "not a valid program")))))

(define scan&parse (sllgen:make-string-parser arith-scanner arith-grammer))
(define run
  (lambda (string env)
    (value-of-program (scan&parse string) env)))
(define value-of-program
  (lambda (pgm env)
    (value-of pgm env)))

(define value-of->val
  (lambda (exp env)
    (expval->val (value-of exp env))))
(define value-of->num
  (lambda (exp env)
    (expval->num (value-of exp env))))
(define value-of->bool
  (lambda (exp env)
    (expval->bool (value-of exp env))))
(define value-of->list
  (lambda (exp env)
    (expval->list (value-of exp env))))
(define lift
  (lambda (lift-f extract-f)
    (lambda (f env . exprs)
    (lift-f (apply
              f
              (map
               (lambda (expr) (extract-f (value-of expr env)))
               exprs))))))

(check-equal? (expval->num
               (run "-(-(x, 3), -(v, i))"
                    (extend-env 'i (num-val 1)
                     (extend-env 'v (num-val 5)
                      (extend-env 'x (num-val 10)
                                  (empty-env))))))
              3)

(check-equal? (expval->num
               (run "/(-(*(x, 3), +(v, i)), 7)"
                    (extend-env 'i (num-val 1)
                     (extend-env 'v (num-val 5)
                      (extend-env 'x (num-val 10)
                                  (empty-env))))))
              (/ 24 7))

(check-equal? (expval->num
               (run "if zero?(-(x, 11)) then -(y, 2) else -(y, 4)"
                    (extend-env 'x (num-val 33)
                     (extend-env 'y (num-val 22)
                      (empty-env)))))
              18)

(check-equal? (expval->num
               (run "if zero?(-(x, 11)) then -(y, 2) else -(y, 4)"
                    (extend-env 'x (num-val 11)
                     (extend-env 'y (num-val 22)
                      (empty-env)))))
              20)

(check-equal? (expval->num
               (run "if greater?(x, 11) then -(y, 2) else -(y, 4)"
                    (extend-env 'x (num-val 33)
                     (extend-env 'y (num-val 22)
                      (empty-env)))))
              20)
(check-equal? (expval->num
               (run "if greater?(x, 11) then -(y, 2) else -(y, 4)"
                    (extend-env 'x (num-val 10)
                     (extend-env 'y (num-val 22)
                      (empty-env)))))
              18)

(check-equal? (expval->num
               (run "if equal?(x, 11) then -(y, 2) else -(y, 4)"
                    (extend-env 'x (num-val 11)
                     (extend-env 'y (num-val 22)
                      (empty-env)))))
              20)
(check-equal? (expval->num
               (run "if equal?(x, 11) then -(y, 2) else -(y, 4)"
                    (extend-env 'x (num-val 10)
                     (extend-env 'y (num-val 22)
                      (empty-env)))))
              18)

(check-equal? (expval->num
               (run "if less?(x, 11) then -(y, 2) else -(y, 4)"
                    (extend-env 'x (num-val 11)
                     (extend-env 'y (num-val 22)
                      (empty-env)))))
              18)
(check-equal? (expval->num
               (run "if less?(x, 11) then -(y, 2) else -(y, 4)"
                    (extend-env 'x (num-val 10)
                     (extend-env 'y (num-val 22)
                      (empty-env)))))
              20)

(check-equal? (expval->num
               (run "let x = 7
                     in let y = 2
                        in let y = let x = -(x, 1) in -(x, y)
                           in -(-(x, 8), y)"
                    (empty-env)))
              -5)

(check-equal? (expval->num
               (run "-(-(minus(x), 3), -(v, i))"
                    (extend-env 'i (num-val 1)
                     (extend-env 'v (num-val 5)
                      (extend-env 'x (num-val 10)
                                  (empty-env))))))
              -17)

(check-equal? (expval->num
               (run "if zero?(-(x, 11)) then -(y, minus(2)) else -(y, 4)"
                    (extend-env 'x (num-val 11)
                     (extend-env 'y (num-val 22)
                      (empty-env)))))
              24)

(check-equal? (expval->num
               (run "let x = 7
                     in let y = minus(2)
                        in let y = let x = -(x, 1) in -(x, y)
                           in -(-(x, 8), y)"
                    (empty-env)))
              -9)
(check-equal? (expval->list
               (run "let x = 4
                     in cons(x, cons(cons(-(x, 1), emptylist), emptylist))"
                    (empty-env)))
              '(4 (3)))
(check-equal? (expval->list
               (run "let x = 4 in list(x, -(x, 1), -(x, 3))"
                    (empty-env)))
              '(4 3 1))
(check-equal? (expval->num
               (run "cond
                       zero?(x) ==> a
                       greater?(y, x) ==> b
                       less?(x, y) ==> c
                     end"
                    (extend-env 'x (num-val 1)
                     (extend-env 'y (num-val 2)
                      (extend-env 'a (num-val 11)
                       (extend-env 'b (num-val 22)
                        (extend-env 'c (num-val 33)
                         (empty-env))))))))
              22)
(check-equal? (expval->num
               (run "let x = 30
                     in let x = -(x, 1)
                            y = -(x, 2)
                        in -(x, y)"
                    (empty-env)))
              1)
(check-equal? (expval->num
               (run "let x = 30
                     in let* x = -(x, 1)
                            y = -(x, 2)
                        in -(x, y)"
                    (empty-env)))
              2)
(check-equal? (expval->num
               (run "let u = 7
                     in unpack x y = cons(u, cons(3, emptylist))
                        in -(x, y)"
                    (empty-env)))
              4)
(check-equal? (expval->num
               (run "let u = 7
                     in unpack x y = list(u, 3)
                        in -(x, y)"
                    (empty-env)))
              4)
