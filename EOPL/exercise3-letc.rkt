#lang eopl
(require rackunit)

(define fold
  (lambda (z append xs)
    (if (null? xs)
        z
        (fold (append z (car xs)) append (cdr xs)))))
(define zip
  (lambda (xs . xss)
    (if (fold #f (lambda (acc pred) (or acc pred)) (map null? (cons xs xss)))
        '()
        (cons
         (map car (cons xs xss))
         (apply zip (map cdr (cons xs xss)))))))

(define empty-env
  (lambda ()
    (lambda (given-var)
      (eopl:error "no binding for " given-var))))
(define extend-env
  (lambda (var val saved-env)
    (lambda (given-var)
      (if (eqv? var given-var)
          val
          (saved-env given-var)))))
(define apply-env
  (lambda (var env)
    (env var)))

(define num-val
  (lambda (val)
    (if (number? val)
        val
        (eopl:error "not a number" val))))
(define bool-val
  (lambda (val)
    (if (boolean? val)
        val
        (eopl:error "not a boolean" val))))
(define proc-val
  (lambda (val)
    (if (procedure? val)
        val
        (eopl:error "not a proc" val))))
(define expval->num
  (lambda (val)
    (if (number? val)
        val
        (eopl:error "not a number" val))))
(define expval->bool
  (lambda (val)
    (if (boolean? val)
        val
        (eopl:error "not a number" val))))
(define expval->proc
  (lambda (val)
    (if (procedure? val)
        val
        (eopl:error "not a number" val))))
(define procedure
  (lambda (vars body bind-env)
    (lambda (vals)
      (value-of body (fold bind-env
                           (lambda (extended-env pair)
                             (extend-env (car pair) (cadr pair) extended-env))
                           (zip vars vals))))))
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
     ("proc" "(" (arbno identifier) ")" expression) proc-exp)
    (expression
     ("zero?" "(" expression ")") zero?-exp)
    (expression
     ("(" expression (arbno expression) ")") apply-exp)
    (expression
     ("let" identifier "=" expression "in" expression) let-exp)
    (expression
     ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression) "in" expression) letrec-exp)
    (expression
     ("if" expression "then" expression "else" expression) if-exp)
    ))
(sllgen:make-define-datatypes arith-scanner arith-grammer)

(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp (num) (num-val num))
           (var-exp (var) (apply-env var env))
           (proc-exp (vars body) (proc-val (procedure vars body env)))
           (letrec-exp (p-names b-varss p-bodys letrec-body) (eval-letrec-exp p-names b-varss p-bodys letrec-body env))
           (apply-exp (rator rands) (eval-apply-exp rator rands env))
           (diff-exp (expr1 expr2) ((lift num-val expval->num) - env  expr1 expr2))
           (if-exp (predicate if-exp false-exp) (eval-if-exp predicate if-exp false-exp env))
           (let-exp (var val-exp body) (eval-let-exp var val-exp body env))
           (zero?-exp (expr) ((lift bool-val expval->num) zero? env expr))
           (else (eopl:error "not a valid expression" exp)))))

(define extend-env-rec
  (lambda (p-name b-vars p-body saved-env)
    (let* ((mutable-env saved-env)
           (val (proc-val (lambda (vals)
                            (value-of p-body (fold mutable-env
                                                 (lambda (extended-env pair)
                                                   (extend-env (car pair) (cadr pair) extended-env))
                                                 (zip b-vars vals)))))))
      (set! mutable-env (extend-env p-name val mutable-env))
      mutable-env)))
(define eval-letrec-exp
  (lambda (p-names b-varss p-bodys letrec-body env)
    (let* ((mutable-env env)
           (acc-env (fold
                     env
                     (lambda (extended-env triple)
                       (let ((p-name (car triple))
                             (b-vars (cadr triple))
                             (p-body (caddr triple)))
                         ;; extend-env with procedures with mutable-env.
                         ;; due to reference by copy, we cannot use a function to simplify this.
                         (extend-env p-name
                                     (proc-val (lambda (vals)
                                                 (value-of p-body (fold mutable-env
                                                                        (lambda (extended-env pair)
                                                                          (extend-env (car pair) (cadr pair) extended-env))
                                                                        (zip b-vars vals)))))
                                     extended-env)))
                     (zip p-names b-varss p-bodys))))
      (set! mutable-env acc-env)
      (value-of letrec-body mutable-env))))
(define eval-apply-exp
  (lambda (rator rands env)
    (let ((proc1 (value-of->proc rator env))
          (vals (map (lambda (val) (value-of val env)) rands)))
      (proc1 vals))))
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
                     in let z = 3
                        in let k = 97
                           in let f = proc (z) -(z, x)
                              in let x = 100
                                 in let g = proc (z) -(z, x)
                                    in -((f 1), (g 1))"
                    (empty-env)))
              -100)

(check-equal? (expval->num
               (run "letrec double(x) = if zero?(x) then 0 else -((double -(x, 1)), -(0, 2))
                     in (double 6)"
                    (empty-env)))
              12)

(check-equal? (expval->num
               (run "letrec times(x y) = if zero?(x) then 0 else -(y, -(0, (times -(x, 1) y)))
                     in (times 3 4)"
                    (empty-env)))
              12)
(check-equal? (expval->num
               (run "letrec
                       even(x) = if zero?(x) then 1 else (odd  -(x, 1))
                       odd(x)  = if zero?(x) then 0 else (even -(x, 1))
                     in (odd 13)"
                    (empty-env)))
              1)
