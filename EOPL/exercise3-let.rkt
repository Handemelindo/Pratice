#lang eopl
(require rackunit)

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
     ("zero?" "(" expression ")") zero?-exp)
    (expression
     ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression
     ("greater?" "(" expression "," expression ")") greater?-exp)
    (expression
     ("less?" "(" expression "," expression ")") less?-exp)
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
     ("null?" "(" expression ")") null?-exp)
    (expression
     ("list" "(" (separated-list expression ",") ")") list-exp)
    (expression
     ("if" expression "then" expression "else" expression) if-exp)
    (expression
     ("cond" (arbno expression "==>" expression) "end") cond-exp)
    (expression
     (identifier) var-exp)
    (expression
     ("let" identifier "=" expression "in" expression) let-exp)))
(sllgen:make-define-datatypes arith-scanner arith-grammer)
(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp (num) (num-val num))
           (var-exp (var) (apply-env var env))
           (diff-exp (expr1 expr2) ((lift num-val expval->num) - env  expr1 expr2))
           (plus-exp (expr1 expr2) ((lift num-val expval->num) + env  expr1 expr2))
           (div-exp (expr1 expr2) ((lift num-val expval->num) / env  expr1 expr2))
           (mul-exp (expr1 expr2) ((lift num-val expval->num) * env  expr1 expr2))
           (equal?-exp (expr1 expr2) ((lift bool-val expval->num) = env expr1 expr2))
           (greater?-exp (expr1 expr2) ((lift bool-val expval->num) > env expr1 expr2))
           (less?-exp (expr1 expr2) ((lift bool-val expval->num) < env  expr1 expr2))
           (zero?-exp (expr) ((lift bool-val expval->num) zero? env expr))
           (minus-exp (expr) ((lift num-val expval->num) - env  expr))
           (emptylist-exp () (list-val '()))
           (cons-exp (expr1 expr2) (eval-cons-exp expr1 expr2 env))
           (car-exp (expr) (val->expval (car (value-of->list expr env))))
           (cdr-exp (expr) (val->expval (cdr (value-of->list expr env))))
           (null?-exp (expr) (bool-val (null? (value-of->list expr env))))
           (list-exp (exprs) (list-val (map (lambda (expr) (value-of->val expr env)) exprs)))
           (if-exp (predicate if-exp false-exp) (eval-if-exp predicate if-exp false-exp env))
           (cond-exp (predicates exprs) (eval-cond-exp predicates exprs env))
           (let-exp (var val-exp body) (eval-let-exp var val-exp body env)))))
(define eval-let-exp
  (lambda (var val-exp body env)
    (let ((val (value-of val-exp env)))
      (let ((new-env (extend-env var val env)))
        (value-of body new-env)))))
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

;; 3.14[**] expression if Bool-exp then Expression else Expression, value-of-bool-exp, obverse what changes accordingly of 3.8
;; 3.15[*] print, and return 1, why it cannot be expressed in specificaton? side effect!!
;; 3.16[**] expression ::= let {identifier = expression}* in expression
;; let x = 30 in let x = 1(x, 1) y = -(x, 2) in -(x, y) should be 1
;; 3.17[**] self indicated let*
;; let x = 30 in let* x = -(x, 1) y = -(x, 2) in -(x, y)
;; 3.18[**] expression ::= unpack {identifier}* = expression in expression,
;; unpack x y z = lst bind x, y, z repectively if lst is list and of size 3, report error otherwise
;; let u = 7 in unpack x y = cons(u, cons(3, emptylist)) in -(x, y) should be 4
;; let u = 7 in unpack x y = list(u, 3) in -(x, y)
