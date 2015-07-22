#lang eopl
(require rackunit)

(define identifier?
  (lambda (ident) (symbol? ident)))

(define-datatype env env?
  (empty-env)
  (extend-env
   (var identifier?)
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
     ("zero?" "(" expression ")") zero?-exp)
    (expression
     ("if" expression "then" expression "else" expression) if-exp)
    (expression
     (identifier) var-exp)
    (expression
     ("let" identifier "=" expression "in" expression) let-exp)))
(sllgen:make-define-datatypes arith-scanner arith-grammer)
(define scan&parse (sllgen:make-string-parser arith-scanner arith-grammer))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))

(define expval->num
  (lambda (val)
    (cases expval val
           (num-val (num) num)
           (else (eopl:error 'num val)))))

(define expval->bool
  (lambda (val)
    (cases expval val
           (bool-val (bool) bool)
           (else (eopl:error "bool" val)))))

(define run
  (lambda (string env)
    (value-of-program (scan&parse string) env)))

(define value-of-program
  (lambda (pgm env)
    (value-of pgm env)))

(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp (num) (num-val num))
           (var-exp (var) (apply-env var env))
           (diff-exp (expr1 expr2)
                     (let ((val1 (value-of expr1 env))
                           (val2 (value-of expr2 env)))
                       (let ((num1 (expval->num val1))
                             (num2 (expval->num val2)))
                         (num-val (- num1 num2)))))
           (zero?-exp (expr)
                      (let ((val (value-of expr env)))
                        (let ((num (expval->num val)))
                          (if (zero? num)
                              (bool-val #t)
                              (bool-val #f)))))
           (if-exp (predicate if-exp false-exp)
                   (let ((val (value-of predicate env)))
                     (let ((pred (expval->bool val)))
                       (if pred
                           (value-of if-exp env)
                           (value-of false-exp env)))))
           (let-exp (var val-exp body)
                    (let ((val (value-of val-exp env)))
                      (let ((new-env (extend-env var val env)))
                        (value-of body new-env)))))))
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
               (run "let x = 7
                     in let y = 2
                        in let y = let x = -(x, 1) in -(x, y)
                           in -(-(x, 8), y)"
                    (empty-env)))
              -5)
