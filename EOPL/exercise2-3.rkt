#lang eopl
(require rackunit)

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               (list-of pred) (cdr val))))))
;;2.29[*]
(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-vars (list-of symbol?))
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rands (list-of lc-exp?))))

(define parse-expression
  (lambda (exp)
    (cond
     ((symbol? exp) (var-exp exp))
     ((pair? exp)
      (if (eqv? (car exp) 'lambda)
          (lambda-exp (cadr exp) (parse-expression (caddr exp)))
          (app-exp
           (parse-expression (car exp))
           (map parse-expression (cdr exp))))))))

(define lc-exp-unparse
  (lambda (exp)
    (cases lc-exp exp
           (var-exp (var) var)
           (lambda-exp (bound-vars body) (list 'lambda bound-vars (lc-exp-unparse body)))
           (app-exp (rator rands) (cons (lc-exp-unparse rator) (map lc-exp-unparse rands))))))

(letrec
    ((iden (var-exp 'x))
     (iden-expr 'x)
     (lambda1 (lambda-exp '(x y z) iden))
     (lambda1-expr '(lambda (x y z) x))
     (app1 (app-exp lambda1 (list (var-exp 'a) (var-exp 'b) (var-exp 'c))))
     (app1-expr '((lambda (x y z) x) a b c)))
  (check-equal? (lc-exp-unparse iden) iden-expr)
  (check-equal? (lc-exp-unparse lambda1) lambda1-expr)
  (check-equal? (lc-exp-unparse app1) app1-expr)
  (check-equal? (lc-exp-unparse (parse-expression iden-expr)) iden-expr)
  (check-equal? (lc-exp-unparse (parse-expression lambda1-expr)) lambda1-expr)
  (check-equal? (lc-exp-unparse (parse-expression app1-expr)) app1-expr))

;;2.30[**]
