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
     (number) num-factor)
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
;; B.3[**]
(define evaluate
  (lambda (arith-expr)
    (cond ((expr? arith-expr)
           (cases expr arith-expr
                  (terms (x ops ys)
                         (fold (evaluate x) + (zipWith (lambda (op y) (op 0 y))
                                            (map evaluate ops)
                                            (map evaluate ys))))))
          ((term? arith-expr)
           (cases term arith-expr
                  (factors (x ops ys)
                           (fold (evaluate x) * (zipWith (lambda (op y) (op 1 y))
                                              (map evaluate ops)
                                              (map evaluate ys))))))
          ((factor? arith-expr)
           (cases factor arith-expr
                  (num-factor (num) num)
                  (expr-factor (expr) (evaluate expr))))
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
  (lambda (x)
    (evaluate (scan&parse x))))
