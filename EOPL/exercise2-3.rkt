#lang eopl
(require rackunit)
(require racket/format)

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               (list-of pred) (cdr val))))))
;;2.29[*] 2.30[**]
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
      (cond
       ((eqv? (car exp) 'lambda)
        (if (= (length exp) 3)
            (lambda-exp (cadr exp) (parse-expression (caddr exp)))
            (eopl:error 'lambda-expression-should-contain-three-parts (~a exp))))
       ((> (length exp) 1)
        (app-exp
           (parse-expression (car exp))
           (map parse-expression (cdr exp))))
       (else
        (eopl:error 'invalid-lambda-expression-of-list (~a exp)))))
     (else (eopl:error 'invalid-lambda-expression (~a exp))))))

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

;;2.31[**]
(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (minuend prefix-exp?)
   (subtrahend prefix-exp?)))

(define polish-parse
  (lambda (tokens)
    (cond
     ((eqv? (car tokens) '-)
      (letrec
          ((r1 (polish-parse (cdr tokens)))
           (fst (car r1))
           (rest1 (cadr r1))
           (r2 (polish-parse rest1))
           (snd (car r2))
           (rest2 (cadr r2)))
           (list (diff-exp fst snd) rest2)))
     ((integer? (car tokens))
      (list (const-exp (car tokens)) (cdr tokens)))
     (else
      (eopl:error 'invalid-symbol)))))

(check-equal? (car (polish-parse '(- - 3 2 - 4 - 12 7)))
              (diff-exp
               (diff-exp
                (const-exp 3)
                (const-exp 2))
               (diff-exp
                (const-exp 4)
                (diff-exp
                 (const-exp 12)
                 (const-exp 7)))))
