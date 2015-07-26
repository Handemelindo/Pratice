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

(define-datatype env env?
  (empty-env)
  (extend-env
   (var symbol?)
   (val expval?)
   (saved-env env?)))
(define apply-env
  (lambda (search-var given-env)
    (cases env given-env
           (extend-env (var val saved-env)
                       (if (eqv? var search-var)
                           val
                           (apply-env search-var saved-env)))
           (else eopl:error "no binding for free variable" search-var given-env))))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?)))
(define-datatype proc proc?
  (procedure
   (var (lambda (v)
          (and (pair? v)
               (fold #t
                     (lambda (z x) (and z x))
                     (map symbol? v)))))
   (body expression?)
   (bind-env env?))
  (trace
   (var (lambda (v)
          (and (pair? v)
               (fold #t
                     (lambda (z x) (and z x))
                     (map symbol? v)))))
   (body expression?)
   (bind-env env?))
  (dprocedure
   (var symbol?)
   (body expression?)))
(define expval->num
  (lambda (val)
    (cases expval val
           (num-val (num) num)
           (else (eopl:error 'num val)))))
(define expval->bool
  (lambda (val)
    (cases expval val
           (bool-val (bool) bool)
           (else (eopl:error 'bool val)))))
(define expval->proc
  (lambda (val)
    (cases expval val
           (proc-val (proc) proc)
           (else (eopl:error 'proc)))))

(define arith-scanner
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))
(define arith-grammer
  '((expression
     (number) const-exp)
    (expression
     ("(-" expression expression ")") diff-exp)
    (expression
     (identifier) var-exp)
    (expression
     ("(proc" (arbno identifier) ")" expression) proc-exp)
    (expression
     ("(dproc" identifier ")" expression) dproc-exp)
    (expression
     ("(traceproc" (arbno identifier) ")" expression) traceproc-exp)
    (expression
     ("(dapply" expression expression")") dapply-exp)
    (expression
     ("(zero?" expression ")") zero?-exp)
    (expression
     ("(" expression (arbno expression) ")") apply-exp)
    (expression
     ("let" identifier "=" expression "in" expression) let-exp)
    (expression
     ("if" expression "then" expression "else" expression) if-exp)
    ))
(sllgen:make-define-datatypes arith-scanner arith-grammer)

(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp (num) (num-val num))
           (var-exp (var) (apply-env var env))
           (proc-exp (vars body) (proc-val (procedure vars body (filter-env-in-exp body vars env (empty-env)))))
           (dproc-exp (var body) (proc-val (dprocedure var body)))
           (traceproc-exp (vars body) (proc-val (trace vars body (filter-env-in-exp body vars env (empty-env)))))
           (apply-exp (rator rands) (eval-apply-exp rator rands env))
           (dapply-exp (rator rands) (eval-dapply-exp rator rands env))
           (diff-exp (expr1 expr2) ((lift num-val expval->num) - env  expr1 expr2))
           (if-exp (predicate if-exp false-exp) (eval-if-exp predicate if-exp false-exp env))
           (let-exp (var val-exp body) (eval-let-exp var val-exp body env))
           (zero?-exp (expr) ((lift bool-val expval->num) zero? env expr))
           (else (eopl:error "not a valid expression" exp)))))
(define contain?
  (lambda (vars var)
    (cond
     ((symbol? vars) (eqv? vars var))
     ((null? vars) #f)
     ((eqv? var (car vars)) #t)
     (else (contain? (cdr vars) var)))))
(define filter-env-in-exp
  (lambda (exp bind env subenv)
    (cases expression exp
           (const-exp (num) subenv)
           (proc-exp (vars body) (filter-env-in-exp body (append bind vars) env subenv))
           (var-exp (var)
                    (if (contain? bind var)
                        subenv
                        (extend-env var (apply-env var env) subenv)))
           (apply-exp (rator rands)
                      (fold subenv
                            (lambda (extended-env subexp)
                              (filter-env-in-exp subexp bind env extended-env))
                            (cons rator rands)))
           (diff-exp (expr1 expr2) (fold subenv
                                         (lambda (extended-env subexp)
                                           (filter-env-in-exp subexp bind env extended-env))
                                         (list expr1 expr2)))
           (if-exp (predicate if-exp false-exp) (fold subenv
                                                      (lambda (extended-env subexp)
                                                        (filter-env-in-exp subexp bind env extended-env))
                                                      (list predicate if-exp false-exp)))
           (let-exp (var val-exp body) (filter-env-in-exp body (append bind var) env subenv))
           (zero?-exp (expr) (filter-env-in-exp expr bind env subenv))
           (else (eopl:error "not a valid expression" exp)))))
(define eval-dapply-exp
  (lambda (rator rand env)
    (let ((proc1 (value-of->proc rator env))
          (val (value-of rand env)))
      (cases proc proc1
             (dprocedure (var body)
                         (value-of body (extend-env var val env)))
             (else (eopl:error "dapply on" rator))))))
(define eval-apply-exp
  (lambda (rator rands env)
    (let ((proc1 (value-of->proc rator env))
          (vals (map (lambda (val) (value-of val env)) rands)))
      (let ((eval-procedure (lambda (vars body bind-env)
                              (if (= (length vars) (length vals))
                                  (value-of body
                                            (fold bind-env
                                                  (lambda (extended-env pair)
                                                    (extend-env (car pair) (cdr pair) extended-env))
                                                  (zip vars vals)))
                                  (eopl:error "vars and vals are not paired" vars vals)))))
        (cases proc proc1
               (procedure (vars body bind-env)
                          (eval-procedure vars body bind-env))
               (trace (vars body bind-env)
                      (display "proc entry")
                      (display (newline))
                      (display "vars: ")
                      (display vars)
                      (display (newline))
                      (display "body: ")
                      (display body)
                      (display (newline))
                      (display "bind-env: ")
                      (display bind-env)
                      (display (newline))
                      (let ((result (eval-procedure vars body bind-env)))
                        (display "proc exit")
                        (display (newline))
                        result))
               (else (eopl:error "eval apply on" proc1)))))))
(define eval-let-exp
  (lambda (var val-exp body env)
    (let ((val (value-of val-exp env)))
      (let ((new-env (extend-env var val env)))
        (value-of body new-env)))))
(define eval-if-exp
  (lambda (predicate if-exp false-exp env)
    (let ((pred (value-of->bool predicate env)))
      (if pred
          (value-of if-exp env)
          (value-of false-exp env)))))

(define scan&parse (sllgen:make-string-parser arith-scanner arith-grammer))
(define run
  (lambda (string env)
    (value-of-program (scan&parse string) env)))
(define value-of-program
  (lambda (pgm env)
    (value-of pgm env)))

(define value-of->num
  (lambda (exp env)
    (expval->num (value-of exp env))))
(define value-of->bool
  (lambda (exp env)
    (expval->bool (value-of exp env))))
(define value-of->proc
  (lambda (exp env)
    (expval->proc (value-of exp env))))
(define lift
  (lambda (lift-f extract-f)
    (lambda (f env . exprs)
    (lift-f (apply
              f
              (map
               (lambda (expr) (extract-f (value-of expr env)))
               exprs))))))

(check-equal? (expval->num
               (run "(- (- x 3) (- v i))"
                    (extend-env 'i (num-val 1)
                     (extend-env 'v (num-val 5)
                      (extend-env 'x (num-val 10)
                                  (empty-env))))))
              3)

(check-equal? (expval->num
               (run "if (zero? (- x 11)) then (- y 2) else (- y 4)"
                    (extend-env 'x (num-val 33)
                     (extend-env 'y (num-val 22)
                      (empty-env)))))
              18)

(check-equal? (expval->num
               (run "if (zero? (- x 11)) then (- y 2) else (- y 4)"
                    (extend-env 'x (num-val 11)
                     (extend-env 'y (num-val 22)
                      (empty-env)))))
              20)


(check-equal? (expval->num
               (run "let x = 7
                     in let y = 2
                        in let y = let x = (- x 1) in (- x y)
                           in (- (- x 8) y)"
                    (empty-env)))
              -5)

(check-equal? (expval->num
               (run "let x = 200
                     in let z = 3
                        in let k = 97
                           in let f = (traceproc z) (- z x)
                              in let x = 100
                                 in let g = (traceproc z) (- z x)
                                    in (- (f 1) (g 1))"
                    (empty-env)))
              -100)

(check-equal? (expval->num
               (run "let add = (proc x y z) (- x (- 0 (- y (- 0 z))))
                     in (add 1 2 3)"
                    (empty-env)))
              6)
(check-equal? (expval->num
               (run "let makemult = (proc maker)
                                      (proc x)
                                        if (zero? x)
                                        then 0
                                        else (- ((maker maker) (- x 1)) (- 0 4))
                     in let times4 = (proc x) ((makemult makemult) x)
                        in (times4 3)"
                    (empty-env)))
              12)
(check-equal? (expval->num
               (run "let factorial = let plus = (proc x y) (- x (- 0 y))
                                     in let times = let maketimes = (proc maker)
                                                                      (proc x y)
                                                                        if (zero? y)
                                                                        then 0
                                                                        else (plus x ((maker maker) x (- y 1)))
                                                    in (maketimes maketimes)
                                        in let makefact = (proc maker)
                                                            (proc x)
                                                              if (zero? x)
                                                              then 1
                                                              else (times x ((maker maker) (- x 1)))
                                           in (makefact makefact)
                     in (factorial 5)"
                    (empty-env)))
              120)
(check-equal? (expval->num
               (run "let makeeven = (proc k1 k2) (proc x) if (zero? x) then 1 else ((k1 k2 k1) (- x 1))
                     in let makeodd = (proc k1 k2) (proc x) if (zero? x) then 0 else ((k1 k2 k1) (- x 1))
                        in let even = (makeeven makeodd makeeven)
                           in let odd = (makeodd makeeven makeodd)
                              in (- (odd 6) (even 6))"
                    (empty-env)))
              -1)
(check-equal? (expval->num
               (run "let y = (proc f)
                               let d = (proc x) (proc z) ((f (x x)) z)
                               in (f (d d))
                     in let maketimes = (proc y) (proc f) (proc x) if (zero? x) then 0 else (- (f (- x 1)) (- 0 y))
                        in let times = (proc i j) ((y (maketimes j)) i)
                           in (times 4 3)"
                    (empty-env)))
              12)
(check-equal? (expval->num
               (run "let a = 3
                     in let p = (dproc x) (- x a)
                        in let a = 5
                           in (- a (dapply p 2))"
                    (empty-env)))
              8)
(check-equal? (expval->num
               (run "let a = 3
                     in let p = (dproc z) a
                        in let f = (dproc x) (dapply p 0)
                           in let a = 5
                              in (dapply f 2)"
                    (empty-env)))
              5)
