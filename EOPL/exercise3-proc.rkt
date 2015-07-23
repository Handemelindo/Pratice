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
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?)))
(define-datatype proc proc?
  (procedure
   (var symbol?)
   (body expression?)
   (bind-env env?)))
(define expval->num
  (lambda (val)
    (cases expval val
           (num-val (num) num)
           (else (eopl:error 'num val)))))
(define expval->bool
  (lambda (val)
    (cases expval val
           (bool-val (bool) bool)
           (else (eopl:error 'bool val)))))
(define expval->proc
  (lambda (val)
    (cases expval val
           (proc-val (proc) proc)
           (else (eopl:error 'proc)))))

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
     (identifier) var-exp)
    (expression
     ("proc" "(" identifier ")" expression) proc-exp)
    (expression
     ("(" expression expression ")") apply-exp)
    (expression
     ("let" identifier "=" expression "in" expression) let-exp)
    (expression
     ("if" expression "then" expression "else" expression) if-exp)
    (expression
     ("zero?" "(" expression ")") zero?-exp)))
(sllgen:make-define-datatypes arith-scanner arith-grammer)

(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp (num) (num-val num))
           (var-exp (var) (apply-env var env))
           (proc-exp (var body) (proc-val (procedure var body env)))
           (apply-exp (rator rand) (eval-apply-exp rator rand env))
           (diff-exp (expr1 expr2) ((lift num-val expval->num) - env  expr1 expr2))
           (if-exp (predicate if-exp false-exp) (eval-if-exp predicate if-exp false-exp env))
           (let-exp (var val-exp body) (eval-let-exp var val-exp body env))
           (zero?-exp (expr) ((lift bool-val expval->num) zero? env expr))
           (else (eopl:error "not a valid expression" exp)))))
(define eval-apply-exp
  (lambda (rator rand env)
    (let ((proc1 (value-of->proc rator env))
          (val (value-of rand env)))
      (cases proc proc1
             (procedure (var body bind-env)
                        (value-of body (extend-env var val bind-env)))))))
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

(define scan&parse (sllgen:make-string-parser arith-scanner arith-grammer))
(define run
  (lambda (string env)
    (value-of-program (scan&parse string) env)))
(define value-of-program
  (lambda (pgm env)
    (value-of pgm env)))

(define value-of->num
  (lambda (exp env)
    (expval->num (value-of exp env))))
(define value-of->bool
  (lambda (exp env)
    (expval->bool (value-of exp env))))
(define value-of->proc
  (lambda (exp env)
    (expval->proc (value-of exp env))))
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
               (run "let x = 7
                     in let y = 2
                        in let y = let x = -(x, 1) in -(x, y)
                           in -(-(x, 8), y)"
                    (empty-env)))
              -5)

(check-equal? (expval->num
               (run "let x = 200
                     in let f = proc (z) -(z, x)
                        in let x = 100
                           in let g = proc (z) -(z, x)
                              in -((f 1), (g 1))"
                    (empty-env)))
              -100)
