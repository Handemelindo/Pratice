#lang racket
(require rackunit)

;;2.1[*]
(define zero-1 (list 0))
(define zero-1? null?)
(define successor-1
  (lambda (x)
    (cond
     ((zero-1? x) (list 1))
     ((< (car x) 15) (cons (+ 1 (car x)) (cdr x)))
     ((not (null? (cdr x))) (cons 0 (successor-1 (cdr x))))
     (else (cons 0 (cons 1 (cdr x)))))))
(define predecessor-1
  (lambda (x)
    (cond
     ((zero-1? x) '())
     ((and (null? (cdr x))
           (eqv? 1 (car x)))
      '())
     ((eqv? 0 (car x)) (cons 15 (predecessor-1 (cdr x))))
     (else (cons (- (car x) 1) (cdr x))))))
(define plus-1
  (lambda (x y)
    (if (or (zero-1? x) (zero-1? y))
        y
        (plus-1 (predecessor-1 x) (successor-1 y)))))
(define multi-1
  (lambda (x y)
    (if (or (zero-1? x) (zero-1? y))
        zero-1
        (plus-1 x (multi-1 x (predecessor-1 y))))))
(define factorial-1
  (lambda (x)
    (if (zero-1? x)
        (successor-1 zero-1)
        (multi-1 (factorial-1 (predecessor-1 x)) x))))

;;2.3[***]
(define zero-2 '(diff (one) (one)))
(define number-2
  (lambda (num)
    (cond
     ((symbol=? 'one (car num)) 1)
     ((symbol=? 'diff (car num)) (- (number-2 (cadr num)) (number-2 (caddr num))))
     (else #f))))
(define zero-2?
  (lambda (num)
    (eqv? (number-2 num) 0)))
(define minus-2
  (lambda (x y)
    (list 'diff x y)))
(define successor-2
  (lambda (num)
    (if (eqv? (car num) 'one)
        '(diff (one) (diff (diff (one) (one)) (one)))
        (minus-2 num (minus-2 zero-2 '(one))))))
(define predecessor-2
  (lambda (num)
    (list 'diff num '(one))))
(define plus
  (lambda (x y)
    (minus-2 x (minus-2 zero-2 y))))

;;implementation of env list
;; empty-env : () -> Env
(define empty-env-0
  (lambda () (list 'empty-env)))

;; ;; extend-env : Var x Val x Env -> Env
(define extend-env-0
  (lambda (var val env)
    (list 'extend-env var val env)))

;; ;; apply-env Var x Env -> Val
(define apply-env-0
  (lambda (var env)
    (cond
     ((eqv? (car env) 'empty-env) 'empty-env)
     ((eqv? (car env) 'extend-env)
      (let
          ((saved-var (cadr env))
           (saved-val (caddr env))
           (next-env (cadddr env)))
        (if (eqv? var saved-var)
            saved-val
            (apply-env-0 var next-env))))
     (else 'no-such-var))))

;; ;; 2.5[*]
(define empty-env-chain
  (lambda () '()))
(define extend-env-chain
  (lambda (var val env)
    (list (list var val) env)))
(define apply-env-chain
  (lambda (var env)
    (if (null? env)
        'no-such-var
        (if (eqv? var (car (car env)))
            (cadr (car env))
            (apply-env-chain var (cadr env))))))
;; ;; 2.8[*]
(define empty-env-chain? null?)

;; ;; 2.9[*]
(define has-binding-chain?
  (lambda (var env)
    (cond
     ((empty-env-chain? env) #f)
     ((eqv? var (car (car env))) #t)
     (else (has-binding-chain? (cadr env))))))

;; ;; 2.10[*]
(define extend-env-chain*
  (lambda (vars vals env)
    (foldl extend-env-chain env vars vals)))

;;2.6[*]
(define empty-env-1
  (lambda () '()))
(define extend-env-1
  (lambda (var val env)
    (cons var (cons val env))))
(define apply-env-1
  (lambda (var env)
    (if (null? env)
        'no-such-var
        (if (eqv? var (car env))
            (cadr env)
            (apply-env-1 var (cddr env))))))

(define empty-env-2
  (lambda () (list '() '())))
(define extend-env-2
  (lambda (var val env)
    (list (cons var (car env)) (cons val (cadr env)))))
(define apply-env-2
  (lambda (var env)
    (cond
     ((and
       (null? (car env))
       (null? (cadr env)))
      'no-such-var)
     ((eqv? var (car (car env))) (car (cadr env)))
     (else (apply-env-2 var (list (cdr (car env)) (cdr (cadr env))))))))

;;2.7[*]
;;TODO

;;2.11[**]
(define empty-env-ribcage
  (lambda () '()))
(define empty-env-ribcage? null?)
(define extend-env-ribcage
  (lambda (var val env)
    (list (list (list var) (list val)) env)))
(define extend-env-ribcage*
  (lambda (vars vals env)
    (list (list vars vals) env)))
(define apply-env-ribcage
  (lambda (var env)
    (if (empty-env-ribcage? env)
        'no-such-var
        ((let*
             ((vars (car (car env)))
              (vals (cadr (car env)))
              (maybe-the-pair (find-var var vars vals)))
           (if (eqv? (car maybe-the-pair) var)
               (cadr maybe-the-pair)
               (apply-env-ribcage var (cadr env))))))))
(define find-var-ribcage
  (lambda (var vars vals)
    (if (null? vars)
        'not-in-this
        (if (eqv? var (car vars))
            (list var (car vals))
            (find-var-ribcage (cdr vars) (cdr vals))))))

;;2.4[**]
(define empty-stack-list
  (lambda () '()))
(define pop-list
  (lambda (stack)
    (if (empty-stack? stack)
        'push-on-empty-stack
        (list
         (car stack)
         (cdr stack)))))
(define push-list
  (lambda (x stack)
    (cons x stack)))
(define top-list
  (lambda (stack)
    (if (empty-stack-list? stack)
        'top-on-empty-stack
        (car stack))))
(define empty-stack-list?
  (lambda (stack)
    (null? stack)))

;;2.12[**]
(define empty-stack-lambda
  (lambda ()
    (lambda () 'empty-stack)))
(define pop-lambda
  (lambda (stack) (stack)))
(define push-lambda
  (lambda (x stack)
    (lambda ()
      (list x stack))))
(define top-lambda
  (lambda (stack)
    (car (pop-lambda stack))))

;;2.13[**]
(define empty-env-lambda
  (lambda ()
    (list (lambda (var) 'empty-env)
          (lambda () #t))))
(define extend-env-lambda
  (lambda (saved-var saved-val saved-env)
    (list
     (lambda (search-var)
       (if (eqv? search-var saved-var)
           saved-val
           ;; use apply-env so that we can throw no-such-var error
           (apply-env-lambda search-var saved-env)))
       (lambda () #f))))
(define apply-env-lambda
  (lambda (var env)
    (if (empty-env-lambda? env)
        'no-such-var
        ((car env) var))))
(define empty-env-lambda?
  (lambda (env)
    ((cadr env))))

;;2.14[**]
(define has-binding-lambda?
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

;;2.16[*]
(define lambda-exp-np
  (lambda (bound-var body)
    (list 'lambda bound-var body)))

(define lambda-exp-np?
  (lambda (lc-exp)
    (and (= (length lc-exp) 3)
         (eqv? (car lc-exp) 'lambda)
         (or (null? (cadr lc-exp))
             (var-exp? (cadr lc-exp))))))

(define lambda-exp-np->bound-var
  (lambda (lc-exp)
    (var-exp->var (cadr lc-exp))))

(define occurs-free-np?
  (lambda (search-var exp)
    (cond
     ((var-exp? exp) (eqv? (var-exp->var exp) search-var))
     ((lambda-exp-np? exp) (and
                         (not (eqv? (lambda-exp-np->bound-var exp) search-var))
                         (occurs-free-np? search-var (lambda-exp->body exp))))
     (else (or
            (occurs-free-np? search-var (app-exp->rator exp))
            (occurs-free-np? search-var (app-exp->rand exp)))))))
(check-equal? 'x (var-exp 'x))
(check-equal? #t (occurs-free-np? 'x 'x))
(check-equal? #f (occurs-free-np? 'x 'y))
(check-equal? '(lambda x (x y))
              (lambda-exp-np 'x (app-exp (var-exp 'x) (var-exp 'y))))
(check-equal? #f (occurs-free-np? 'x '(lambda x (x y))))
(check-equal? #t (occurs-free-np? 'x '(lambda y (x y))))
(check-equal? '((lambda x x) (x y))
              (app-exp (lambda-exp-np 'x (var-exp 'x))
                       (app-exp (var-exp 'x) (var-exp 'y))))
(check-equal? #t (occurs-free-np? 'x '((lambda x x) (x y))))
(check-equal? '(lambda y (lambda z (x (y z))))
              (lambda-exp-np 'y
                          (lambda-exp-np 'z
                                      (app-exp
                                       (var-exp 'x)
                                       (app-exp
                                        (var-exp 'y)
                                        (var-exp 'z))))))
(check-equal? #t (occurs-free-np? 'x '(lambda y (lambda z (x (y z))))))

;;2.17[*]
;;TODO

;;2.18[*]
;; extractor
(define current-element
  (lambda (lst)
    (car lst)))
(check-equal? 6 (current-element '(6 (5 4 3 2 1) (7 8 9))))

(define current-left
  (lambda (lst)
    (cadr lst)))
(check-equal? '(5 4 3 2 1) (current-left '(6 (5 4 3 2 1) (7 8 9))))

(define current-right
  (lambda (lst)
    (caddr lst)))
(check-equal? '(7 8 9) (current-right '(6 (5 4 3 2 1) (7 8 9))))

;; predicates
(define at-left-end?
  (lambda (lst)
    (null? (current-left lst))))
(check-equal? #f (at-left-end? '(6 (5 4 3 2 1) (7 8 9))))
(check-equal? #t (at-left-end? '(6 () (7 8 9))))
(define at-right-end?
  (lambda (lst)
    (null? (current-right lst))))
(check-equal? #f (at-right-end? '(6 (5 4 3 2 1) (7 8 9))))
(check-equal? #t (at-right-end? '(6 (5 4 3 2 1) ())))

;;constructor
(define number->sequence
  (lambda (num)
    (list num '() '())))
(check-equal? '(7 () ()) (number->sequence 7))

(define insert-to-left
  (lambda (lst num)
    (list
     (current-element lst)
     (cons num (current-left lst))
     (current-right lst))))
(check-equal? '(6 (13 5 4 3 2 1) (7 8 9)) (insert-to-left '(6 (5 4 3 2 1) (7 8 9)) 13))

(define insert-to-right
  (lambda (lst num)
    (list (current-element lst) (current-left lst) (cons num (current-right lst)))))
(check-equal? '(6 (5 4 3 2 1) (13 7 8 9)) (insert-to-right '(6 (5 4 3 2 1) (7 8 9)) 13))

(define move-to-left
  (lambda (lst)
    (if (at-left-end? lst)
        'left-on-left-end
        (list
             (car (current-left lst))
             (cdr (current-left lst))
             (cons (current-element lst) (current-right lst))))))
(check-equal? '(5 (4 3 2 1) (6 7 8 9)) (move-to-left '(6 (5 4 3 2 1) (7 8 9))))

(define move-to-right
  (lambda (lst)
    (if (at-right-end? lst)
        'right-on-right-end
        (list
            (car (current-right lst))
            (cons (current-element lst) (current-left lst))
            (cdr (current-right lst))))))
(check-equal? '(7 (6 5 4 3 2 1) (8 9)) (move-to-right '(6 (5 4 3 2 1) (7 8 9))))

;;[2.19*]
;; extractor
(define bintree-current-element
  (lambda (bt) (car bt)))

(define bintree-move-to-left
  (lambda (bt)
    (if (bintree-at-leaf? bt)
        'left-on-leaf
        (cadr bt))))

(define bintree-move-to-right
  (lambda (bt)
    (if (bintree-at-leaf? bt)
        'right-on-leaf
        (caddr bt))))

;; predicate
(define bintree-at-leaf?
  (lambda (bt)
    (null? bt)))

;; constructor
(define empty-bintree
  (lambda () '()))

(define number->bintree
  (lambda (x) (list x (empty-bintree) (empty-bintree))))

(define bintree-insert-to-left
  (lambda (x bt)
    (list
     (bintree-current-element bt)
     (list x (bintree-move-to-left bt) (empty-bintree))
     (bintree-move-to-right bt))))

(define bintree-insert-to-right
  (lambda (x bt)
    (list
     (bintree-current-element bt)
     (bintree-move-to-left bt)
     (list x (empty-bintree) (bintree-move-to-right bt)))))

(define t1 (bintree-insert-to-right 14
              (bintree-insert-to-left 12
                 (number->bintree 13))))

(check-equal? '(13 () ()) (number->bintree 13))
(check-equal? '(13
                (12 () ())
                (14 () ())) t1)
(check-equal? '(12 () ()) (bintree-move-to-left t1))
(check-equal? 14 (bintree-current-element (bintree-move-to-right t1)))
(check-equal? (bintree-at-leaf? (bintree-move-to-right (bintree-move-to-left t1))) #t)
(check-equal? (bintree-at-leaf? (bintree-move-to-right t1)) #f)
(check-equal? '(13
                (15
                 (12 () ())
                 ())
                (14 () ()))
              (bintree-insert-to-left 15 t1))
