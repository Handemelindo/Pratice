#lang eopl
(require rackunit)

;; 2.21[*]
(define-datatype env env?
  (empty-env)
  (extend-env
   (var (lambda (x) (symbol? x)))
   (val (lambda (x) #t))
   (saved-env env?)))

(define has-binding?
  (lambda (search-var given-env)
    (cases env given-env
           (extend-env (var val saved-env)
                       (if (eqv? var search-var)
                           #t
                           (has-binding? search-var saved-env)))
           (else #f))))
(check-equal? (has-binding? 'a (extend-env 'a 1 (empty-env))) #t)
(check-equal? (has-binding? 'b (extend-env 'a 1 (empty-env))) #f)

;; 2.22[*]
(define-datatype stack stack?
  (empty-stack)
  (nonempty-stack
   (top (lambda (x) #t))
   (rest (lambda (x) #t))))
(define push
  (lambda (x s) (nonempty-stack x s)))
(define pop
  (lambda (s)
    (cases stack s
           (empty-stack () 'pop-on-empty)
           (nonempty-stack (top rest) (list top rest)))))
(define top
  (lambda (s)
    (cases stack s
           (empty-stack () 'top-on-empty)
           (nonempty-stack (top rest) top))))
(define empty-stack?
  (lambda (s)
    (cases stack s
           (empty-stack () #t)
           (else #f))))

(letrec
    ((stack1 (push 1 (empty-stack)))
     (stack2 (push 2 stack1))
     (result1 (pop stack1))
     (result2 (pop stack2)))
  (check-equal? (empty-stack? (empty-stack)) #t)
  (check-equal? (empty-stack? stack1) #f)
  (check-equal? (empty-stack? (cadr result1)) #t)
  (check-equal? (empty-stack? (cadr result2)) #f)
  (check-equal? (car result1) 1)
  (check-equal? (car result2) 2)
  (check-equal? (top stack1) 1)
  (check-equal? (top stack2) 2))
