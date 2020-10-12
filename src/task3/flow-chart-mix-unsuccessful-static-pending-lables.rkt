
(module fc-mix racket
  
  (provide flow-chart-mix)
  (provide envHasKey envAssoc setEnv updateEnv substitute is-evaluable try-eval)

  ;;;; ENV UTILITIES
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
  (define (substitute env expr [quoted #f]) (match expr
                                  [`',_ expr] ; ignore double quoted literals
                                  [expr (match expr ; otherwize apply induction over list constructor
                                          [(cons head tail) (cons (substitute env head quoted) (substitute env tail quoted))]
                                          [_ (if (envHasKey expr env)
                                                 (if quoted `',(envAssoc expr env) (envAssoc expr env))
                                                 expr)])
                                        ])
    )

  ;; checks that expression 'expr' can be evaluated with respect to dynamic variables 'dynamic'
    (define (is-evaluable dynamic expr) (match expr
                                  [`',_ #t] ; ignore double quoted literals
                                  [expr (match expr ; otherwize apply induction over list constructor
                                          [(cons head tail) (and (is-evaluable dynamic head) (is-evaluable dynamic tail))]
                                          [_ (if (member expr dynamic) #f #t)])
                                        ])
    )

  
  ;; tries to evaluate expression 'expr' within sandbox 'sandbox'. If it fails, returns it unchanged.
  (define (try-eval env expr) (let ([exprSub (substitute env expr #t)]) (with-handlers ([exn:fail?
                                                                                      (λ (e) (begin (displayln e) exprSub))])
                                                                       (eval exprSub))
                                )
    )
  ;;;; ENV UTILITIES

; 1. Flow chart specializer written on flow chart for flow chart

; data definition:
(define division-example '((a b c) (x y z))) ;; BOTH static and dynamic should be specified
(define vs0-example '(0 1 "asdf"))
; Returns tail from element, or #f if not found:
;  (member 'y division-example)

(define flow-chart-mix
  '(
    ;; program input data + initialization block
    (read program division vs0)
    (init
          (:= env (map list (car division) vs0)) ; [D] create initial env for computations (zip (car division) and vs0)
              
          (:= pp0 (caadr program)) ; [S] get initial label
          (:= pending (set `(,pp0 . ,vs0))) ; set initial state (label . values static)
          (:= pending-lables (list pp0)) ; [S] variable containing lables for bounded variants of pending lables (The Trick)
          (:= pending-values (list vs0)) ; [D] variable containing static values of static variables
          ;;; !!! Thus, pending = (zip pending-lables pending-values)!
          (:= marked (set)) ; [D] create set of marked specialized blocks
          (:= residual '()) ; [D] create stub of the specialized programm (result of the mix)
          (goto while-1-cond)
          )

    ;; while pending != <empty> we have blocks 'pp' to specialize with static values from 'vs'
    (while-1-cond (if (empty? pending-lables) halt while-1-body-1))
    (while-1-body-1 (:= spec-state (set-first pending))  ; extract pair (pp . vs) into 'spec-state'
                    (:= pp (car pending-lables))             ; [D] take synchronously pair (pp . vs) from
                    (:= vs (car pending-values))             ; [D] splitted structure (zip pending-lables pending-values)
                    (:= env (updateEnv (map list (car division) vs) env))
                    ; (:= pending    (set-rest pending))   ; delete pair from pending set (or maybe not)
                    (:= pending-lables (cdr pending-lables))             ; [S] Delete pair
                    (:= pending-values (cdr pending-values))             ; [D]  from pending set
                    ; double-check that we have already specialized that block
                    (if (set-member? marked `(,pp . ,vs))
                        while-1-cond ; immediate continuation (`continue` operator)
                        while-1-body-2))
    (while-1-body-2
                    (:= _ (println (format "specializing: ~s; (cddr vs)=~s" pp (cddr vs))))
                    (:= marked     (set-add marked `(,pp . ,vs))) ; [D] also add extracted pair to 'marked' ones
                    ;; NOTE: inline function find basic block 'bb' by the name of the label 'pp'
                    (:= labeled-block (assoc pp program)) ; [S] extract labeled basic block
                    (:= bb (cdr labeled-block))          ; [S] extract basic block (with goto)
                    ;; create buffer for commands 'code'. Algorithm suggests to make it labeled
                    (:= code `((,pp . ,vs))) ; [D]
                    (goto while-2-cond) ; goto inner 'while-2' block (condition section)
                    )
    (while-1-body-3 (:= residual (cons code residual))  ; when cycle 'while-2' is over, add generated block into residual with label 'pp'
                    (goto while-1-cond)                 ; then move back to 'while-1' condition checking
                    )

    ;; while bb != <empty> analyze every incoming statement and build 'code' specialized block
    (while-2-cond ;(:= _ (begin (println ">>> while2.code=") (println code)))
                  (if (empty? bb) while-1-body-3 while-2-body))
    (while-2-body (:= command (car bb)) ; [S] chop next command from basic block 'bb'
                  (:= bb (cdr bb))      ; assign the rest to the basic block 'bb' itself
                  (goto while-2-body-switch-case-1-1) ; switch-case on 'command' type
                  )
    ;; while-2-body-switch-case
        (while-2-body-switch-case-1-1
                  (if (equal? (car command) ':=) while-2-body-switch-case-1-assign while-2-body-switch-case-1-2)
                  )
        (while-2-body-switch-case-1-2
                  (if (equal? (car command) 'if) while-2-body-switch-case-1-if while-2-body-switch-case-1-3)
                  )
        (while-2-body-switch-case-1-3
                  (if (equal? (car command) 'goto) while-2-body-switch-case-1-goto while-2-body-switch-case-1-4)
                  )
        (while-2-body-switch-case-1-4
                  (if (equal? (car command) 'return) while-2-body-switch-case-1-return while-2-body-switch-case-1-default)
                  )
        (while-2-body-switch-case-1-default
                  (goto error-no-such-command) ;; error: pattern matching failed, command is unknown
                  )
        
        ;; := (assignment)
        (while-2-body-switch-case-1-assign
         (:= X (cadr command))   ; [S] extract a variable of assignment
         (:= expr (caddr command)) ; [S] extract an expression of assignment
         ; (:= _ (println (format "division=~s, X=~s, is_static=~s " division X (member X (car division)))))
         (if (member X (car division)) ; if X is static by division (expr should be static)
             while-2-body-switch-case-1-assign-static
             while-2-body-switch-case-1-assign-dynamic)
         )
        
        (while-2-body-switch-case-1-assign-static
         ;(:= _ (begin (print "static assign of: ") (displayln expr)))
         (:= X-newval (try-eval env expr))
         ;(:= _ (begin (print "static assign result: ") (displayln X-newval)))
         (:= env (setEnv X X-newval env))
         (goto while-2-cond)
         )
        (while-2-body-switch-case-1-assign-dynamic
         ;(:= _ (begin (print "dynamic-partial-eval of: ") (displayln expr)))
         (:= newexpr (substitute env expr #t))
         ;(:= _ (begin (print "dynamic-partial-eval result: ") (displayln newexpr)))
         (:= code (append code `((:= ,X ,newexpr))))
         (goto while-2-cond)
         )

        
        ;; goto
        (while-2-body-switch-case-1-goto
         (:= next-label (cadr command)) ; [S] extract new label name
         (goto while-2-transition-compression)
         )

         ;; if
        (while-2-body-switch-case-1-if
         (:= predicate (cadr command))   ; [S] extract a branching condition
         (:= pp-true (caddr command))    ; [S] true label
         (:= pp-false (cadddr command))   ; [S] false label
         
         ;(:= _ (begin (print "while-2-body-switch-case-1-if predicate=") (display predicate)))
         ;(:= _ (begin (print "; env=") (display env)))
         ;(:= _ (begin (print "; is-evaluatable=") (display (is-evaluable (cadr division) predicate))))
         ;(:= _ (begin (print "; eval=") (displayln (try-eval env predicate))))
         (if (is-evaluable (cadr division) predicate) ; if X is static by division (!!!)
             while-2-body-switch-case-1-if-static
             while-2-body-switch-case-1-if-dynamic)
         )
        
        (while-2-body-switch-case-1-if-static
         ;(:= _ (begin (print "while-2-body-switch-case-1-if-static predicate=") (display predicate)))
         ;(:= _ (begin (print "; env=") (display env)))
         ;(:= _ (begin (print "; evaluates in=") (displayln (try-eval env predicate))))
         (if (try-eval env predicate)
             while-2-body-switch-case-1-if-static-true
             while-2-body-switch-case-1-if-static-false
         ))
        (while-2-body-switch-case-1-if-static-true
         (:= next-label pp-true)
         (goto while-2-transition-compression)
         )
        (while-2-body-switch-case-1-if-static-false
         (:= next-label pp-false)
         (goto while-2-transition-compression)
         )
        
        (while-2-body-switch-case-1-if-dynamic
         ;(:= _ (begin (print "dynamic-partial-eval of: ") (displayln predicate)))
         (:= new_predicate (substitute env predicate #t)) ; [D]
         ;(:= _ (begin (print "dynamic-partial-eval result: ") (displayln new_predicate)))
         ;(:= _ (begin (print "partial-eval of: ") (displayln (car division))))
         (:= new_vs (substitute env (car division)))
         ;(:= _ (begin (print "dynamic-partial-eval result: ") (displayln new_vs)))
         (:= label_true `(,pp-true . ,new_vs))
         (:= label_false `(,pp-false . ,new_vs))
         ; reset as most static variables as we can
         (:= pp '())
         (:= labeled-block '())
         (:= bb '())
         (:= command '())
         (:= X '())
         (:= expr '())

         (if (set-member? pending label_true)
             while-2-body-switch-case-1-if-dynamic-tryset-false
             while-2-body-switch-case-1-if-dynamic-set-true)
         )
        (while-2-body-switch-case-1-if-dynamic-set-true
         (:= pending (set-add pending label_true))
         (:= pending-lables (cons pp-true pending-lables))
         (:= pending-values (cons new_vs pending-values))
         ;(:= _ (println (format "pending-lables-true=~s" pending-lables)))
         (goto while-2-body-switch-case-1-if-dynamic-tryset-false)
         )
        (while-2-body-switch-case-1-if-dynamic-tryset-false
         (if (set-member? pending label_false)
             while-2-body-switch-case-1-if-dynamic-end
             while-2-body-switch-case-1-if-dynamic-set-false)
         )
        (while-2-body-switch-case-1-if-dynamic-set-false
         (:= pending (set-add pending label_false))
         (:= pending-lables (cons pp-false pending-lables))
         (:= pending-values (cons new_vs pending-values))
         ;(:= _ (println (format "pending-lables-false=~s" pending-lables)))
         (goto while-2-body-switch-case-1-if-dynamic-end)
         )
        (while-2-body-switch-case-1-if-dynamic-end
         (:= new_expr `(if ,new_predicate ,label_true ,label_false))
         (:= code (append code `(,new_expr)))
         ; (:= _ (begin (print "current pending:") (displayln pending)))
         (goto while-2-cond)
         )
         

         
         ;; return
        (while-2-body-switch-case-1-return
         (:= expr (cadr command))   ; extract return statement
         (:= newexpr (substitute env expr #t)) ; specialize it
         (:= new_stmt `(return ,newexpr)) ; construct new statement
         (:= code (append code `(,new_stmt))) ; add it to 'poly'
         (goto while-2-cond) ; and go back to while-2 loop
         )
         
    ;; END while-2-body-switch-case
          

          ;; input parameter: next-label
          (while-2-transition-compression
           ;; NOTE: inline function find basic block 'bb' by the name of the label 'next-label'
           (:= labeled-block (assoc next-label program)) ; extract labeled basic block
           (:= bb (cdr labeled-block))          ; extract basic block (with goto)
           ; transition compression happened
           (goto while-2-cond)
           )
    
    ; program end
    (halt
     (:= residual (cons `(__init0 (goto (,pp0 . ,vs0))) residual)) ; append jump to initial block
     (:= header (cdar program)) ; get header of the program
     (:= header (remove* (car division) header)) ; remove static variables from the program list
     (:= header (append '(read) header)) ; add 'read' heading to the program
     (:= residual (cons header residual)) ; left the rest for the specializer
     (return residual)
     )

    ; error handlers
    (error-no-such-command (return "NO SUCH COMMAND"))
    )
  )
  )