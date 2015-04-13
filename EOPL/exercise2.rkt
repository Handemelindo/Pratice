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
(define empty-env
  (lambda () '()))
(define empty-env? null?)
(define extend-env
  (lambda (var val env)
    (list (list (list var) (list val)) env)))
(define extend-env*
  (lambda (vars vals env)
    (list (list vars vals) env)))
(define apply-env
  (lambda (var env)
    (if (empty-env? env)
        'no-such-var
        ((let*
             ((vars (car (car env)))
              (vals (cadr (car env)))
              (maybe-the-pair (find-var var vars vals)))
           (if (eqv? (car maybe-the-pair) var)
               (cadr maybe-the-pair)
               (apply-env var (cadr env))))))))
(define find-var
  (lambda (var vars vals)
    (if (null? vars)
        'not-in-this
        (if (eqv? var (car vars))
            (list var (car vals))
            (find-var (cdr vars) (cdr vals))))))
