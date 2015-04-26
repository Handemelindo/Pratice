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

;;2.23[*]
(define-datatype lc-exp lc-exp?
  (var-exp
   (var (lambda (x) (not (lc-exp? x)))))
  (lambda-exp
   (bound-var (lambda (x) (not (lc-exp? x))))
   (boby lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(letrec
    ((iden (var-exp 'x))
     (expr (lambda-exp 'x iden))
     ;; (invalid-iden (var-exp expr))
     ;; (invalid-expr (lambda-exp expr iden))
     (app (app-exp iden expr)))
  (check-equal? (lc-exp? iden) #t)
  (check-equal? (lc-exp? expr) #t)
  ;; (check-equal? (lc-exp? invalid-iden) #f)
  ;; (check-equal? (lc-exp? invalid-expr) #f)
  (check-equal? (lc-exp? app) #t))

;;2.24[*]
(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define bintree-to-list
  (lambda (bt)
    (cases bintree bt
           (leaf-node (num) (list 'leaf-node num))
           (interior-node (key left right)
                          (list
                           'interior-node
                           key
                           (bintree-to-list left)
                           (bintree-to-list right))))))

(let
    ((t1 (leaf-node 2))
     (t2 (interior-node 'a (leaf-node 3) (leaf-node 4))))
  (check-equal? (bintree-to-list t1) '(leaf-node 2))
  (check-equal? (bintree-to-list t2) '(interior-node a (leaf-node 3) (leaf-node 4))))

;;2.25[**] TODO use memorization
(define max-interior
  (lambda (bt)
    (cases bintree bt
           (leaf-node (num) #f)
           (interior-node (key left right)
                          (cases bintree left
                                 (leaf-node (num-left)
                                            (cases bintree right
                                                   ;; leaf leaf
                                                   (leaf-node (num-right)
                                                              (list key (+ num-left num-right) (+ num-left num-right)))
                                                   ;; leaf interior
                                                   (interior-node (key-right left-right right-right)
                                                                  (letrec
                                                                      ((result (max-interior right))
                                                                       (acc (cadr result))
                                                                       (best (caddr result))
                                                                       (best-key (car result)))
                                                                    (if (>= (+ acc num-left) best)
                                                                      (list key (+ acc num-left) (+ acc num-left))
                                                                      (list best-key (+ acc num-left) best))))))
                                 (interior-node (key-left left-left right-left)
                                                (cases bintree right
                                                       ;; interior leaf
                                                       (leaf-node (num-right)
                                                                  (letrec
                                                                      ((result (max-interior left))
                                                                       (acc (cadr result))
                                                                       (best (caddr result))
                                                                       (best-key (car result)))
                                                                    (if (>= (+ acc num-right) best)
                                                                      (list key (+ acc num-right) (+ acc num-right))
                                                                      (list best-key (+ acc num-right) best))))
                                                       ;; interior interior
                                                       (interior-node (key-right left-right right-right)
                                                                      (letrec
                                                                          ((result-left (max-interior left))
                                                                           (result-right (max-interior right))
                                                                           (key-left (car result-left))
                                                                           (key-right (car result-right))
                                                                           (acc (+ (cadr result-left) (cadr result-right)))
                                                                           (max-left (cadr result-left))
                                                                           (max-right (cadr result-right)))
                                                                        (if (< acc max-left)
                                                                            (if (< max-left max-right)
                                                                                (list key-right acc max-right)
                                                                                (list key-left acc max-left))
                                                                            (if (< acc max-right)
                                                                                (list key-right acc max-right)
                                                                                (list key acc acc))))))))))))

(letrec
    ((tree-1
      (interior-node 'foo (leaf-node 2) (leaf-node 3)))
     (tree-2
      (interior-node 'bar (leaf-node -1) tree-1))
     (tree-3
      (interior-node 'baz tree-2 (leaf-node 1))))
  (check-equal? (car (max-interior tree-1)) 'foo)
  (check-equal? (car (max-interior tree-2)) 'foo)
  (check-equal? (car (max-interior tree-3)) 'baz))

;;2.26[**]
(define-datatype blue-subtree blue-subtree?
  (empty-blue-subtree)
  (nonempty-blue-subtree
   (head red-blue-tree?)
   (tail blue-subtree?)))
(define-datatype red-blue-tree red-blue-tree?
  (red-node
   (left red-blue-tree?)
   (right red-blue-tree?))
  (blue-node
   (trees blue-subtree?))
  (rbt-leaf-node
   (num integer?)))

(define mark-blue-subtree
  (lambda (bst acc)
    (cases blue-subtree bst
           (empty-blue-subtree () '())
           (nonempty-blue-subtree (head tail)
                                  (cons
                                   (mark-rbt-depth head acc)
                                   (mark-blue-subtree tail acc))))))
(define mark-rbt-depth
  (lambda (rbt acc)
    (cases red-blue-tree rbt
           (red-node (left right)
                     (list 'red (mark-rbt-depth left (+ acc 1)) (mark-rbt-depth right (+ acc 1))))
           (blue-node (trees) (cons 'blue (mark-blue-subtree trees acc)))
           (rbt-leaf-node (num) acc))))

(letrec
    ((t1 (red-node (rbt-leaf-node 1) (rbt-leaf-node 2)))
     (r1 '(red 1 1))
     (t2 (blue-node (nonempty-blue-subtree (rbt-leaf-node 1) (empty-blue-subtree))))
     (r2 '(blue 0))
     (t3 (blue-node (empty-blue-subtree)))
     (r3 '(blue))
     (t4 (red-node t2 t3))
     (r4 '(red
           (blue 1)
           (blue)))
     (t5 (blue-node (nonempty-blue-subtree t4 (nonempty-blue-subtree t1 (empty-blue-subtree)))))
     (r5 '(blue
           (red
            (blue 1)
            (blue))
           (red 1 1)))
     (t6 (red-node t5 t4))
     (r6 '(red
           (blue
            (red
             (blue 2)
             (blue))
            (red 2 2))
           (red
            (blue 2)
            (blue))))
     (t7 (blue-node (nonempty-blue-subtree t4
                     (nonempty-blue-subtree t2
                      (nonempty-blue-subtree (red-node t2 t1) (empty-blue-subtree))))))
     (r7 '(blue
           (red
            (blue 1)
            (blue))
           (blue 0)
           (red
            (blue 1)
            (red 2 2)))))
  (check-equal? (mark-rbt-depth t1 0) r1)
  (check-equal? (mark-rbt-depth t2 0) r2)
  (check-equal? (mark-rbt-depth t3 0) r3)
  (check-equal? (mark-rbt-depth t4 0) r4)
  (check-equal? (mark-rbt-depth t5 0) r5)
  (check-equal? (mark-rbt-depth t6 0) r6))

;;2.28[*]
(define lc-exp-unparse
  (lambda (exp)
    (cases lc-exp exp
           (var-exp (var) (symbol->string var))
           (lambda-exp (bound-var body) (string-append
                                         "lambda (" (symbol->string bound-var) ") "
                                         (lc-exp-unparse body)))
           (app-exp (rator rand) (string-append
                                  "("
                                  (lc-exp-unparse rator)
                                  " "
                                  (lc-exp-unparse rand)
                                  ")")))))

(letrec
    ((iden (var-exp 'x))
     (expr (lambda-exp 'x iden))
     (app (app-exp iden expr)))
  (check-equal? (lc-exp-unparse iden) "x")
  (check-equal? (lc-exp-unparse expr) "lambda (x) x")
  (check-equal? (lc-exp-unparse app) "(x lambda (x) x)"))
