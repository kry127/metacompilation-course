#lang racket

;; environment format
(define envExample '((x 1) (y 2) (z 3)))
(define envExample+ '((x 5) (z 7)))
(define constExpr '(+ x (+ y z)))
(define partialExpr '(+ (+ w x) (+ y z)))

;; checks that key is in the environment
(define (envHasKey name env) (match env
  [(cons e env) (if (equal? (car e) name) #t (envHasKey name env))]
  [_ #f]))

;; same as (assoc name env)
(define (envAssoc name env) (match env
  [(cons e env) (if (equal? (car e) name) (cadr e) (envAssoc name env))]
  [_ error "Key not found"]))

;; add name=val to environment
(define (setEnv name val env) (match env
  [(cons e env) (if (equal? (car e) name) (cons (list name val) env) (cons e (setEnv name val env)))]
  [_ (cons (list name val) '())]))

;; update env with mappint from newEnv
(define (updateEnv newEnv env) (match newEnv
                                 [(cons e newEnv) (setEnv (car e) (cadr e) (updateEnv newEnv env))]
                                 [_ env]))

;; partially substitutes env variables into expression
(define (substitute env expr) (match expr
                        [`',_ expr] ; ignore double quoted literals
                        [expr (match expr ; otherwize apply induction over list constructor
                           [(cons head tail) (cons (substitute env head) (substitute env tail))]
                           [_ (if (envHasKey expr env) (envAssoc expr env) expr)])
                         ])
  )

;; checks that expression 'expr' can be evaluated in environment 'env'
(define (is-evaluable env expr) 
                          (with-handlers ([exn:fail?
                                           (λ (e) #f)])
                            (begin `(eval ,(substitute env expr)) #t))
                          )

  
;; tries to evaluate expression 'expr' within sandbox 'sandbox'. If it fails, returns it unchanged.
(define (try-eval env expr) (let ([exprSub (substitute env expr)]) (with-handlers ([exn:fail?
                                                                                (λ (e) exprSub)])
                                                                 (eval exprSub))
                              )
  )

