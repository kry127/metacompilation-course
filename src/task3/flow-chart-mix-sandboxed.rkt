
(module fc-mix racket
  
  (require racket/sandbox)
  
  (provide flow-chart-mix)
  (provide flow-chart-mix is-evaluable try-eval dynamic-partial-eval)

;; checks that expression 'expr' can be evaluated in sandbox 'sandbox'
(define (is-evaluable sandbox expr) 
                          (with-handlers ([exn:fail?
                                           (λ (e) #f)])
                            (begin (sandbox `(eval ,expr)) #t))
                          )

  
;; tries to evaluate expression 'expr' within sandbox 'sandbox'. If it fails, returns it unchanged.
(define (try-eval sandbox expr) (with-handlers ([exn:fail?
                                           (λ (e) expr)])
                            (sandbox `(eval ,expr)))
                          )

(define (is-identifier sandbox expr) (with-handlers ([exn:fail?
                                           (λ (e) #f)])
                            (begin (sandbox expr) #t))
                          )
  
(define (try-open-identifier sandbox expr) (with-handlers ([exn:fail?
                                           (λ (e) expr)])
                            (sandbox expr))
                          )


;; partially evaluates the expression
(define (dynamic-partial-eval sandbox s-expr) (match s-expr
                                                [`',_ s-expr] ; ignore double quoted literals
                                                [s-expr (if (is-evaluable sandbox s-expr) ; if expression is fully evaluatable
                                                            (try-eval sandbox s-expr) ; then 'try' to evaluate it
                                                            (match s-expr ; otherwize apply induction over list constructor
                                                              [(cons head tail) (cons (dynamic-partial-eval sandbox head) (dynamic-partial-eval sandbox tail))]
                                                              [_ (if (is-identifier sandbox s-expr) (try-open-identifier sandbox s-expr) s-expr)])
                                                            )]
                                                )
  )

; 1. Flow chart specializer written on flow chart for flow chart

; data definition:
(define division-example '(x y z))
(define vs0-example '(0 1 "asdf"))
; Returns tail from element, or #f if not found:
;  (member 'y division-example)

(define flow-chart-mix
  '(
    ;; program input data + initialization block
    (read program division vs0)
    (init
          (:= sandbox (make-evaluator 'racket)) ; create sandbox for computations
          (:= _ (sandbox '(require racket/sandbox))) ; add dependencies to the sandbox
          ; first of all, assign all static variables listed in 'division' corresponding value from 'vs0' in sandbox environment
          ;   see issue here: https://stackoverflow.com/questions/28947041/unbound-identifier-racket-operators
          (:= _ (sandbox `(eval '(define-values ,division (apply values ',vs0)))))
          (:= _ (sandbox `(eval '(define division ',division ))))
              
          (:= pp0 (caadr program)) ; get initial label
          (:= pending (set `(,pp0 . ,vs0))) ; set initial state (label . values static)
          ; (:= _ (begin (println "DISPLAY pp0: ") (displayln pp0)))
          ; (:= _ (begin (println "DISPLAY vs0: ") (displayln vs0)))
          ; (:= _ (begin (println "DISPLAY pending: ") (displayln pending)))
          (:= marked (set)) ; create set of marked specialized blocks
          (:= residual '()) ; create stub of the specialized programm (result of the mix)
          (goto while-1-cond)
          )

    ;; while pending != <empty> we have blocks 'pp' to specialize with static values 'vs' of variables 'division'
    (while-1-cond (if (set-empty? pending) halt while-1-body-1))
    (while-1-body-1 (:= spec-state (set-first pending))  ; extract pair (pp . vs) into 'spec-state'
                    (:= pp (car spec-state))             ; destruct 'spec-state' and extract 'pp'
                    (:= vs (cdr spec-state))             ; destruct 'spec-state' and extract 'vs'
                    (:= _ (sandbox `(eval '(define-values ,division (apply values ',vs)))))
                    (:= pending    (set-rest pending))   ; and delete it from pending set
                    (:= marked     (set-add marked spec-state)) ; also add extracted pair to 'marked' ones
                    ;; NOTE: inline function find basic block 'bb' by the name of the label 'pp'
                    (:= labeled-block (assq pp program)) ; extract labeled basic block
                    (:= bb (cdr labeled-block))          ; extract basic block (with goto)
                    ;; create buffer for commands 'code'. Algorithm suggests to make it labeled
                    (:= code `((,pp . ,vs)))
                    (goto while-2-cond) ; goto inner 'while-2' block (condition section)
                    )
    (while-1-body-2 (:= residual (cons code residual))  ; when cycle 'while-2' is over, add generated block into residual with label 'pp'
                    (goto while-1-cond)                 ; then move back to 'while-1' condition checking
                    )

    ;; while bb != <empty> analyze every incoming statement and build 'code' specialized block
    (while-2-cond ;(:= _ (begin (println ">>> while2.code=") (println code)))
                  (if (empty? bb) while-1-body-2 while-2-body))
    (while-2-body (:= command (car bb)) ; chop next command from basic block 'bb'
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
         (:= X (cadr command))   ; extract a variable of assignment
         (:= expr (caddr command)) ; extract an expression of assignment
         (if (member X division) ; if X is static by division (expr should be static)
             while-2-body-switch-case-1-assign-static
             while-2-body-switch-case-1-assign-dynamic)
         )
        
        (while-2-body-switch-case-1-assign-static
         ;(:= _ (begin (print "static assign of: ") (displayln expr)))
         (:= X-newval (sandbox `(eval ',expr)))
         (:= _ (sandbox `(define ,X ',X-newval)))
         (goto while-2-cond)
         )
        (while-2-body-switch-case-1-assign-dynamic
         ;(:= _ (begin (print "dynamic-partial-eval of: ") (displayln expr)))
         (:= newexpr (dynamic-partial-eval sandbox expr))
         ;(:= _ (begin (print "dynamic-partial-eval result: ") (displayln newexpr)))
         (:= code (append code `((:= ,X ,newexpr))))
         (goto while-2-cond)
         )

        
        ;; goto
        (while-2-body-switch-case-1-goto
         (:= next-label (cadr command)) ; extract new label name
         (goto while-2-transition-compression)
         )

         ;; if
        (while-2-body-switch-case-1-if
         (:= predicate (cadr command))   ; extract a branching condition
         (:= pp-true (caddr command))    ; true label
         (:= pp-false (cadddr command))   ; false label
         (if  (is-evaluable sandbox predicate) ; if X is static by division
             while-2-body-switch-case-1-if-static
             while-2-body-switch-case-1-if-dynamic)
         )
        
        (while-2-body-switch-case-1-if-static
         (:= next-label (if  (sandbox `(eval ,predicate)) pp-true pp-false))
         (goto while-2-transition-compression)
         )
        
        (while-2-body-switch-case-1-if-dynamic
         ;(:= _ (begin (print "dynamic-partial-eval of: ") (displayln predicate)))
         (:= new_predicate (dynamic-partial-eval sandbox predicate))
         ;(:= _ (begin (print "dynamic-partial-eval result: ") (displayln new_predicate)))
         ;(:= _ (begin (print "partial-eval of: ") (displayln division)))
         (:= new_vs (dynamic-partial-eval sandbox `,division))
         ;(:= _ (begin (print "dynamic-partial-eval result: ") (displayln new_vs)))
         (:= label_true `(,pp-true . ,new_vs))
         (:= label_false `(,pp-false . ,new_vs))
         (:= pending (if (set-member? marked label_true) pending  (set-add pending label_true)))
         (:= pending (if (set-member? marked label_false) pending (set-add pending label_false)))
         ;(:= _ (begin (print "current pending:") (displayln pending)))
         (:= new_expr `(if ,new_predicate ,label_true ,label_false))
         (:= code (append code `(,new_expr)))
         (goto while-2-cond)
         )

         
         ;; return
        (while-2-body-switch-case-1-return
         (:= expr (cadr command))   ; extract return statement
         ; (:= newexpr (sandbox `(partial-eval ,expr ,division))) ; specialize it
         (:= newexpr (dynamic-partial-eval sandbox expr)) ; specialize it
         (:= new_stmt `(return ,newexpr)) ; construct new statement
         (:= code (append code `(,new_stmt))) ; add it to 'poly'
         (goto while-2-cond) ; and go back to while-2 loop
         )
         
    ;; END while-2-body-switch-case
          

          ;; input parameter: next-label
          (while-2-transition-compression
           ;; NOTE: inline function find basic block 'bb' by the name of the label 'next-label'
           (:= labeled-block (assq next-label program)) ; extract labeled basic block
           (:= bb (cdr labeled-block))          ; extract basic block (with goto)
           ; transition compression happened
           (goto while-2-cond)
           )
    
    ; program end
    (halt
     (:= residual (cons `(__init0 (goto (,pp0 . ,vs0))) residual)) ; append jump to initial block
     (:= header (cdar program)) ; get header of the program
     (:= header (remove* division header)) ; remove static variables from the program list
     (:= header (append '(read) header)) ; add 'read' heading to the program
     (:= residual (cons header residual)) ; left the rest for the specializer
     (return residual)
     )

    ; error handlers
    (error-no-such-command (return "NO SUCH COMMAND"))
    )
  )
  )