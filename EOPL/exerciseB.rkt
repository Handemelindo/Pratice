#lang eopl
(require rackunit)

;; B.1[*]
(define-datatype arith arith?
  (expr
   (expr )))
(define arith-scanner
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))
(define arith-grammer
  '((expr
     (term (arbno add-op term)) expr)
    (term
     (factor (arbno mul-op factor)) factor)
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
(define list-the-datatype
  (lambda ()
    (sllgen:list-define-datatypes arith-scanner arith-grammer)))
(define scan&parse
  (sllgen:make-string-parser arith-scanner arith-grammer))
(scan&parse "3 + (2 * 66) - 5")
(struct:expr
  #(struct:factor #(struct:number 3) () ())
  (#(struct:add) #(struct:minus))
  (#(struct:factor #(struct:number 2) (#(struct:mul)) (#(struct:number 66))) #(struct:factor #(struct:number 5) () ())))
