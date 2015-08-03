#lang eopl
(require rackunit)

(define zip
  (lambda (xs ys)
    (if (or (null? xs) (null? ys))
        '()
        (cons
         (cons (car xs) (car ys))
         (zip (cdr xs) (cdr ys))))))
(define fold
  (lambda (z append xs)
    (if (null? xs)
        z
        (fold (append z (car xs)) append (cdr xs)))))

(define list-of
  (lambda (p)
    (lambda (list)
      (and (map p list)))))
(define-datatype nameless nameless?
  (nameless-const-exp
   (num number?))
  (nameless-var-exp
   (n number?))
  (nameless-let-exp
   (exp nameless?)
   (body nameless?))
  (nameless-proc-exp
   (body nameless?))
  (nameless-diff-exp
   (exp1 nameless?)
   (exp2 nameless?))
  (nameless-apply-exp
   (rator nameless?)
   (rand nameless?))
  (nameless-if-exp
   (b-exp nameless?)
   (t-exp nameless?)
   (f-exp nameless?))
  (nameless-cond-exp
   (p-exps (list-of nameless?))
   (exps (list-of nameless?)))
  (nameless-zero?-exp
   (exp nameless?)))
(define empty-senv
  (lambda () '()))
(define extend-senv
  (lambda (var senv)
    (cons var senv)))
(define apply-senv
  (lambda (var senv)
    (cond ((null? senv) (eopl:error "cannot find" var))
          ((eqv?  (car senv) var) 0)
          (else (+ 1 (apply-senv var (cdr senv)))))))
(define nameless-env?
  (lambda (env)
    ((list-of expval?) env)))
(define empty-nameless-env
  (lambda ()
    '()))
(define extend-nameless-env
  (lambda (var env)
    (cons var env)))
(define apply-nameless-env
  (lambda (n env)
    (list-ref env n)))

(define empty-env
  (lambda () '()))
(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))
(define apply-env
  (lambda (var env)
    (cond ((null? env) (eopl:error "cannot find" var))
          ((eqv? (car (car env)) var) (cdr (car env)))
          (else (apply var (cdr env))))))
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc procedure?)))
(define expval->num
  (lambda (val)
    (cases expval val
           (num-val (num) num)
           (else (eopl:error "not a number" val)))))
(define expval->bool
  (lambda (val)
    (cases expval val
           (bool-val (bool) bool)
           (else (eopl:error "not a bool" val)))))
(define expval->proc
  (lambda (val)
    (cases expval val
           (proc-val (proc) proc)
           (else (eopl:error "not a proc" val)))))

(define arith-scanner
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))
(define arith-grammer
  '((expression
     (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")") diff-exp)
    (expression
     (identifier) var-exp)
    (expression
     ("proc" "(" identifier ")" expression) proc-exp)
    (expression
     ("(" expression expression")") apply-exp)
    (expression
     ("let" identifier "=" expression "in" expression) let-exp)
    (expression
     ("if" expression "then" expression "else" expression) if-exp)
    (expression
     ("cond" (arbno expression "==>" expression) "end") cond-exp)
    (expression
     ("zero?" "(" expression ")") zero?-exp)))
(sllgen:make-define-datatypes arith-scanner arith-grammer)
(define scan&parse (sllgen:make-string-parser arith-scanner arith-grammer))

(define translation-of
  (lambda (expr senv)
    (cases expression expr
           (const-exp (num)
                      (nameless-const-exp num))
           (diff-exp (exp1 exp2)
                     (nameless-diff-exp
                      (translation-of exp1 senv)
                      (translation-of exp2 senv)))
           (var-exp (var)
                    (nameless-var-exp (apply-senv var senv)))
           (proc-exp (bind-var body)
                     (nameless-proc-exp (translation-of
                                         body
                                         (extend-senv bind-var senv))))
           (apply-exp (rator rand)
                      (nameless-apply-exp
                       (translation-of rator senv)
                       (translation-of rand senv)))
           (let-exp (var val body)
                    (nameless-let-exp
                     (translation-of val senv)
                     (translation-of body (extend-senv var senv))))
           (if-exp (b-exp t-exp f-exp)
                   (nameless-if-exp
                    (translation-of b-exp senv)
                    (translation-of t-exp senv)
                    (translation-of f-exp senv)))
           (cond-exp (p-exps exps)
                     (nameless-cond-exp
                      (map (lambda (expr) (translation-of expr senv)) p-exps)
                      (map (lambda (expr) (translation-of expr senv)) exps)))
           (zero?-exp (expr)
                      (nameless-zero?-exp (translation-of expr senv)))
           (else (eopl:error "invalid expression" expr)))))

(define apply-proc
  (lambda (rator val)
    (rator val)))
(define procedure
  (lambda (body bind-env)
    (lambda (val)
      (value-of body (extend-nameless-env val bind-env)))))

(define value-of
  (lambda (exp env)
    (cases nameless exp
           (nameless-const-exp (num) (num-val num))
           (nameless-diff-exp (exp1 exp2) (eval-diff-exp exp1 exp2 env))
           (nameless-var-exp (n) (apply-nameless-env n env))
           (nameless-proc-exp (body) (proc-val (procedure body env)))
           (nameless-apply-exp (rator rand) (eval-apply-exp rator rand env))
           (nameless-let-exp (val body) (eval-let-exp val body env))
           (nameless-if-exp (p-exp t-exp f-exp) (eval-if-exp p-exp t-exp f-exp env))
           (nameless-cond-exp (p-exps exps) (eval-cond-exp p-exps exps env))
           (nameless-zero?-exp (expr) (bool-val (zero? (expval->num (value-of expr env)))))
           (else (eopl:error "invalid expression" exp)))))
(define eval-diff-exp
  (lambda (exp1 exp2 env)
    (num-val (-
              (expval->num (value-of exp1 env))
              (expval->num (value-of exp2 env))))))
(define eval-apply-exp
  (lambda (rator rand env)
    (let ((proc (expval->proc (value-of rator env)))
          (val (value-of rand env)))
      (proc val))))
(define eval-let-exp
  (lambda (val body env)
    (value-of body (extend-nameless-env
                    (value-of val env)
                    env))))
(define eval-if-exp
  (lambda (p-exp t-exp f-exp env)
    (let ((pred (expval->bool (value-of p-exp env))))
                              (if pred
                                  (value-of t-exp env)
                                  (value-of f-exp env)))))
(define eval-cond-exp
  (lambda (p-exps exps env)
    (if (or (null? p-exps) (null? exps))
        (eopl:error "none of predicates meet")
        (let ((predicate (expval->bool (value-of (car p-exps) env))))
          (if predicate
              (value-of (car exps) env)
              (eval-cond-exp (cdr p-exps) (cdr exps) env))))))

(define run
  (lambda (pgm env)
    (value-of (translation-of (scan&parse pgm) (map car env)) (map cdr env))))

(check-equal? (expval->num
               (run "-(-(x, 3), -(v, i))"
                    (extend-env 'i (num-val 1)
                     (extend-env 'v (num-val 5)
                      (extend-env 'x (num-val 10)
                       (empty-env))))))
              3)

(check-equal? (expval->num
               (run "if zero?(-(x, 11)) then -(y, 2) else -(y, 4)"
                    (extend-env 'x (num-val 33)
                     (extend-env 'y (num-val 22)
                      (empty-env)))))
              18)

(check-equal? (expval->num
               (run "if zero?(-(x, 11)) then -(y, 2) else -(y, 4)"
                    (extend-env 'x (num-val 11)
                     (extend-env 'y (num-val 22)
                      (empty-env)))))
              20)

(check-equal? (expval->num
               (run "let x = 7
                     in let y = 2
                        in let y = let x = -(x, 1) in -(x, y)
                           in -(-(x, 8), y)"
                    (empty-env)))
              -5)

(check-equal? (expval->num
               (run "let x = 200
                     in let z = 3
                        in let k = 97
                           in let f = proc (z) -(z, x)
                              in let x = 100
                                 in let g = proc (z) -(z, x)
                                    in -((f 1), (g 1))"
                    (empty-env)))
              -100)

(check-equal? (expval->num
               (run "cond
                       zero?(x) ==> a
                       zero?(y) ==> b
                       zero?(z) ==> c
                     end"
                    (extend-env 'x (num-val 1)
                     (extend-env 'y (num-val 0)
                      (extend-env 'z (num-val 0)
                       (extend-env 'a (num-val 11)
                        (extend-env 'b (num-val 22)
                         (extend-env 'c (num-val 33)
                          (empty-env)))))))))
              22)
