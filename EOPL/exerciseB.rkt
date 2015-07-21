#lang eopl
(require rackunit)

(define fold
  (lambda (z append xs)
    (if (null? xs)
        z
        (append (car xs) (fold z append (cdr xs))))))

(define zipWith
  (lambda (f xs ys)
    (cond
     ((null? xs) '())
     ((null? ys) '())
     (else
      (cons
       (f (car xs) (car ys))
       (zipWith f (cdr xs) (cdr ys)))))))

;; B.1[*]
(define arith-scanner
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))
(define arith-grammer
  '((expr
     (term (arbno add-op term)) terms)
    (term
     (factor (arbno mul-op factor)) factors)
    (factor
     ("-" number) neg-factor)
    (factor
     (number) num-factor)
    (factor
     (identifier) var-factor)
    (factor
     ("(" expr ")") expr-factor)
    (add-op
     ("+") add)
    (add-op
     ("-") minus)
    (mul-op
     ("*") mul)
    (mul-op
     ("/") div)))
;; B.3, B.4, B.5[**]
(define-datatype env env?
  (empty-env)
  (extend-env
   (var symbol?)
   (val (lambda (x) #t))
   (saved-env env?)))
(define apply-env
  (lambda (search-var given-env)
    (cases env given-env
           (extend-env (var val saved-env)
                       (if (eqv? var search-var)
                           val
                           (apply-env search-var saved-env)))
           (else eopl:error "no binding for free variable"))))
(define evaluate
  (lambda (arith-expr env)
    (cond ((expr? arith-expr)
           (cases expr arith-expr
                  (terms (x ops ys)
                         (fold (evaluate x env) + (zipWith (lambda (op y) (op 0 y))
                                                       (map (lambda (z) (evaluate z env)) ops)
                                                       (map (lambda (z) (evaluate z env)) ys))))))
          ((term? arith-expr)
           (cases term arith-expr
                  (factors (x ops ys)
                           (fold (evaluate x env) * (zipWith (lambda (op y) (op 1 y))
                                                         (map (lambda (z) (evaluate z env)) ops)
                                                         (map (lambda (z) (evaluate z env)) ys))))))
          ((factor? arith-expr)
           (cases factor arith-expr
                  (neg-factor (num) (- num))
                  (num-factor (num) num)
                  (var-factor (var) (apply-env var env))
                  (expr-factor (expr) (evaluate expr env))))
          ((add-op? arith-expr)
           (cases add-op arith-expr
                  (add () +)
                  (minus () -)))
          ((mul-op? arith-expr)
           (cases mul-op arith-expr
                  (mul () *)
                  (div () /)))
          (else (eopl:error "illegal operator")))))
(sllgen:make-define-datatypes arith-scanner arith-grammer)
(define scan&parse (sllgen:make-string-parser arith-scanner arith-grammer))
(define scan (sllgen:make-string-scanner arith-scanner arith-grammer))
(define scan&parse&eval
  (lambda (x env)
    (evaluate (scan&parse x) env)))
(check-equal? (scan&parse&eval "2 + 3 * 5 - 6"
                               (empty-env))
              11)
(check-equal? (scan&parse&eval "a + 3 * b - c"
                               (extend-env 'a 2
                                           (extend-env 'b 5
                                                       (extend-env 'c 6 (empty-env)))))
              11)
(check-equal? (scan&parse&eval "2 + 3 * -5"
                               (empty-env))
              -13)
(check-equal? (scan&parse&eval "-2 + 3 * 5"
                               (empty-env))
              13)
(check-equal? (scan&parse&eval "2 + -3 * 5"
                               (empty-env))
              -13)
