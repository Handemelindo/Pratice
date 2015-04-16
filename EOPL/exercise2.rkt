#lang racket
(require rackunit)

;;2.1[*]
;; (define zero (list 0))
;; (define zero? null?)
;; (define successor
;;   (lambda (x)
;;     (cond
;;      ((zero? x) (list 1))
;;      ((< (car x) 15) (cons (+ 1 (car x)) (cdr x)))
;;      ((not (null? (cdr x))) (cons 0 (successor (cdr x))))
;;      (else (cons 0 (cons 1 (cdr x)))))))
;; (define predecessor
;;   (lambda (x)
;;     (cond
;;      ((zero? x) '())
;;      ((and (null? (cdr x))
;;            (eqv? 1 (car x)))
;;       '())
;;      ((eqv? 0 (car x)) (cons 15 (predecessor (cdr x))))
;;      (else (cons (- (car x) 1) (cdr x))))))
;; (define plus
;;   (lambda (x y)
;;     (if (or (zero? x) (zero? y))
;;         y
;;         (plus (predecessor x) (successor y)))))
;; (define multi
;;   (lambda (x y)
;;     (if (or (zero? x) (zero? y))
;;         zero
;;         (plus x (multi x (predecessor y))))))
;; (define factorial
;;   (lambda (x)
;;     (if (zero? x)
;;         (successor zero)
;;         (multi (factorial (predecessor x)) x))))

;;2.3[***]
;; (define zero '(diff (one) (one)))
;; (define number
;;   (lambda (num)
;;     (cond
;;      ((symbol=? 'one (car num)) 1)
;;      ((symbol=? 'diff (car num)) (- (number (cadr num)) (number (caddr num))))
;;      (else #f))))
;; (define zero?
;;   (lambda (num)
;;     (eqv? (number num) 0)))
;; (define minus
;;   (lambda (x y)
;;     (list 'diff x y)))
;; (define successor
;;   (lambda (num)
;;     (if (eqv? (car num) 'one)
;;         '(diff (one) (diff (diff (one) (one)) (one)))
;;         (minus num (minus zero '(one))))))
;; (define predecessor
;;   (lambda (num)
;;     (list 'diff num '(one))))
;; (define plus
;;   (lambda (x y)
;;     (minus x (minus zero y))))

;;implementation of env list
;; ;; empty-env : () -> Env
;; (define empty-env
;;   (lambda () (list 'empty-env)))

;; ;; extend-env : Var x Val x Env -> Env
;; (define extend-env
;;   (lambda (var val env)
;;     (list 'extend-env var val env)))

;; ;; apply-env Var x Env -> Val
;; (define apply-env
;;   (lambda (var env)
;;     (cond
;;      ((eqv? (car env) 'empty-env) 'empty-env)
;;      ((eqv? (car env) 'extend-env)
;;       (let
;;           ((saved-var (cadr env))
;;            (saved-val (caddr env))
;;            (next-env (cadddr env)))
;;         (if (eqv? var saved-var)
;;             saved-val
;;             (apply-env var next-env))))
;;      (else 'no-such-var))))

;; ;; 2.5[*]
;; (define empty-env
;;   (lambda () '()))
;; (define extend-env
;;   (lambda (var val env)
;;     (list (list var val) env)))
;; (define apply-env
;;   (lambda (var env)
;;     (if (null? env)
;;         'no-such-var
;;         (if (eqv? var (car (car env)))
;;             (cadr (car env))
;;             (apply-env var (cadr env))))))
;; ;; 2.8[*]
;; (define empty-env? null?)

;; ;; 2.9[*]
;; (define has-binding?
;;   (lambda (var env)
;;     (cond
;;      ((empty-env? env) #f)
;;      ((eqv? var (car (car env))) #t)
;;      (else (has-binding? (cadr env))))))

;; ;; 2.10[*]
;; (define extend-env*
;;   (lambda (vars vals env)
;;     (foldl extend-env env vars vals)))

;;2.6[*]
;; (define empty-env
;;   (lambda () '()))
;; (define extend-env
;;   (lambda (var val env)
;;     (cons var (cons val env))))
;; (define apply-env
;;   (lambda (var env)
;;     (if (null? env)
;;         'no-such-var
;;         (if (eqv? var (car env))
;;             (cadr env)
;;             (apply-env var (cddr env))))))

;; (define empty-env
;;   (lambda () (list '() '())))
;; (define extend-env
;;   (lambda (var val env)
;;     (list (cons var (car env)) (cons val (cadr env)))))
;; (define apply-env
;;   (lambda (var env)
;;     (cond
;;      ((and
;;        (null? (car env))
;;        (null? (cadr env)))
;;       'no-such-var)
;;      ((eqv? var (car (car env))) (car (cadr env)))
;;      (else (apply-env var (list (cdr (car env)) (cdr (cadr env))))))))

;;2.7[*]
;;TODO

;;2.11[**]
;; (define empty-env
;;   (lambda () '()))
;; (define empty-env? null?)
;; (define extend-env
;;   (lambda (var val env)
;;     (list (list (list var) (list val)) env)))
;; (define extend-env*
;;   (lambda (vars vals env)
;;     (list (list vars vals) env)))
;; (define apply-env
;;   (lambda (var env)
;;     (if (empty-env? env)
;;         'no-such-var
;;         ((let*
;;              ((vars (car (car env)))
;;               (vals (cadr (car env)))
;;               (maybe-the-pair (find-var var vars vals)))
;;            (if (eqv? (car maybe-the-pair) var)
;;                (cadr maybe-the-pair)
;;                (apply-env var (cadr env))))))))
;; (define find-var
;;   (lambda (var vars vals)
;;     (if (null? vars)
;;         'not-in-this
;;         (if (eqv? var (car vars))
;;             (list var (car vals))
;;             (find-var (cdr vars) (cdr vals))))))

;;2.4[**]
;; (define empty-stack
;;   (lambda () '()))
;; (define pop
;;   (lambda (stack)
;;     (if (empty-stack? stack)
;;         'push-on-empty-stack
;;         (list
;;          (car stack)
;;          (cdr stack)))))
;; (define push
;;   (lambda (x stack)
;;     (cons x stack)))
;; (define top
;;   (lambda (stack)
;;     (if (empty-stack? stack)
;;         'top-on-empty-stack
;;         (car stack))))
;; (define empty-stack?
;;   (lambda (stack)
;;     (null? stack)))

;;2.12[**]
(define empty-stack
  (lambda ()
    (lambda () 'empty-stack)))
(define pop
  (lambda (stack) (stack)))
(define push
  (lambda (x stack)
    (lambda ()
      (list x stack))))
(define top
  (lambda (stack)
    (car (pop stack))))

;;2.13[**]
(define empty-env
  (lambda ()
    (list (lambda (var) 'empty-env)
          (lambda () #t))))
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list
     (lambda (search-var)
       (if (eqv? search-var saved-var)
           saved-val
           ;; use apply-env so that we can throw no-such-var error
           (apply-env search-var saved-env)))
       (lambda () #f))))
(define apply-env
  (lambda (var env)
    (if (empty-env? env)
        'no-such-var
        ((car env) var))))
(define empty-env?
  (lambda (env)
    ((cadr env))))

;;2.14[**]
(define has-binding?
  (lambda (var env)
    (not (eqv? 'no-such-var (apply-env var env)))))

;;2.15[*]
;;var-exp : Var -> Lc-exp
;;usage: (var-exp var) = a lambda calculus simply contains a var
(define var-exp
  (lambda (var) var))

;;lambda-exp : Var x Lc-exp -> Lc-exp
;;usage: (lambda-exp bound-var body) = a lambda expression that contains a body and bounded variable
(define lambda-exp
  (lambda (bound-var body)
    (list 'lambda (list bound-var) body)))

;;app-exp : Lc-exp x Lx-exp -> Lc-exp
;;usage: (app-exp rator rand) = a lambda expression that apply the operator on the operand
(define app-exp
  (lambda (rator rand)
    (list rator rand)))

;;var-exp? : Lc-exp -> Bool
;;usage: (var-exp? lc-exp) = ture if the lambda expression is a var
(define var-exp?
  (lambda (lc-exp)
    (symbol? lc-exp)))

;;lambda-exp? : Lc-exp -> Bool
;;usage: (lambda-exp? lc-exp) = true if the lambda expression is a lambda
(define lambda-exp?
  (lambda (lc-exp)
    (and (= (length lc-exp) 3)
         (eqv? (car lc-exp) 'lambda)
         (or (null? (cadr lc-exp))
             (var-exp? (car (cadr lc-exp)))))))

;;app-exp? : Lc-exp -> Bool
;;usage: (app-exp? lc-exp) = true if the lambda expression is application of a lambda
(define app-exp?
  (lambda (lc-exp)
    (and (= (length lc-exp) 2)
         (lambda-exp? (car lc-exp))
         (lambda-exp? (cadr lc-exp)))))

;;var-exp->var : Lc-exp -> Var
;;usage: (var-exp->var lc-exp) = the var of a lambda expression only contains a var
(define var-exp->var
  (lambda (lc-exp)
    lc-exp))

;;lambda-exp->bound-var : Lc-exp -> Var
;;usage: (lambda-exp->bound-var lc-exp) = extract the bounded var in this lambda expression
(define lambda-exp->bound-var
  (lambda (lc-exp)
    (var-exp->var (car (cadr lc-exp)))))

;;lambda-exp->body : Lc-exp -> Lc-exp
;;usage: (lambda-exp->body lc-exp) = the body of this lambda expression
(define lambda-exp->body
  (lambda (lc-exp)
    (caddr lc-exp)))

;;app-exp->rator : Lc-exp -> Lc-exp
;;usage: (app-exp->rator lc-exp) = the operator of this lambda application
(define app-exp->rator
  (lambda (lc-exp)
    (car lc-exp)))

;;app-exp->rand : Lc-exp -> Lc-exp
;;usage: (app-exp-rand lc-exp) = the operand of this lambda application
(define app-exp->rand
  (lambda (lc-exp)
    (cadr lc-exp)))
;;occurs-free? : Sym x LcExp -> Bool
(define occurs-free?
  (lambda (search-var exp)
    (cond
     ((var-exp? exp) (eqv? (var-exp->var exp) search-var))
     ((lambda-exp? exp) (and
                         (not (eqv? (lambda-exp->bound-var exp) search-var))
                         (occurs-free? search-var (lambda-exp->body exp))))
     (else (or
            (occurs-free? search-var (app-exp->rator exp))
            (occurs-free? search-var (app-exp->rand exp)))))))
(check-equal? 'x (var-exp 'x))
(check-equal? #t (occurs-free? 'x 'x))
(check-equal? #f (occurs-free? 'x 'y))
(check-equal? '(lambda (x) (x y))
              (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'y))))
(check-equal? #f (occurs-free? 'x '(lambda (x) (x y))))
(check-equal? #t (occurs-free? 'x '(lambda (y) (x y))))
(check-equal? '((lambda (x) x) (x y))
              (app-exp (lambda-exp 'x (var-exp 'x))
                       (app-exp (var-exp 'x) (var-exp 'y))))
(check-equal? #t (occurs-free? 'x '((lambda (x) x) (x y))))
(check-equal? '(lambda (y) (lambda (z) (x (y z))))
              (lambda-exp 'y
                          (lambda-exp 'z
                                      (app-exp
                                       (var-exp 'x)
                                       (app-exp
                                        (var-exp 'y)
                                        (var-exp 'z))))))
(check-equal? #t (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z))))))
