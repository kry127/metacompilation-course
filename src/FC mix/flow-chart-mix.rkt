
(module fc-mix racket
  
  (provide flow-chart-mix)
  (provide envHasKey envAssoc setEnv updateEnv substitute is-evaluable try-eval
           find-blocks-in-pending label-generator-factory zip)

  ;;;; ENV UTILITIES
  (define (zip lst1 lst2) (begin (map list lst1 lst2)))
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
                                          [(cons head tail) `(,(substitute env head quoted) ,@(map (lambda (t) (substitute env t quoted)) tail))]
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

  ;; extracts set of static variables that occurs in expression
    (define (extract-used-statics static expr) (match expr
                                  [`',_ (set)] ; ignore double quoted literals
                                  [expr (match expr ; otherwize apply induction over list constructor
                                          [(cons head tail) (set-union (extract-used-statics static head) (extract-used-statics static tail))]
                                          [_ (if (member expr static) (set expr) (set))])
                                        ])
    )

  
  ;; tries to evaluate expression 'expr' within sandbox 'sandbox'. If it fails, returns it unchanged.
  (define (try-eval env expr [quoted #t]) (let ([exprSub (substitute env expr quoted)]) (with-handlers ([exn:fail?
                                                                                      (λ (e) (begin ;;; (displayln e)
                                                                                                    exprSub))])
                                                                       (begin ;; (println (format "expr=~s, eval=~s, env=~s" exprSub (eval exprSub) env))
                                                                              (eval exprSub)))
                                )
    )
  ;;;; ENV UTILITIES


  ;;;; THE TRICK UTILITIES
  ; this function computes 'blocks-in-pending' set described in article
  (define (recbb dynamic bb) (match bb
                                                                             [(cons `(if ,expr ,label-true ,label-false) xs)
                                                                              (if (is-evaluable dynamic expr) (set) (set label-true label-false))]
                                                                             [(cons x xs) (recbb dynamic xs)]
                                                                             ['() (set)]
                                                                             ))
  (define (recbbs dynamic bbs) (match bbs
                                                                             [(cons b bs) (set-union (recbb dynamic b) (recbbs dynamic bs))]
                                                                             ['() (set)]
                                                                              ))
  (define (find-blocks-in-pending dynamic program) (recbbs dynamic (cdr program)))
  ;;;; THE TRICK UTILITIES


  
  ;;;; LIFE VARIABLE ANALYSIS UTILITIES
  
  ; takes program, label and environment (staticvar |-> val mapping), returns static variables that are sound to this flow
  ; visited is a set of visited lables
  (define (life-static-variables program label env dynamic [visited (set)]) (if (set-member? visited label) (set) (life-static-variables-bb program (cdr (assoc label program)) env dynamic (set-add visited label))))

  (define (life-static-variables-bb program bb env dynamic visited)  (match bb
                                                       ['() (error "Empty basic block")]
                                                       [(cons command bbs) (match command
                                                                             [`(:= ,var ,expr) (set-union (extract-used-statics (map car env) expr) (life-static-variables-bb program bbs env dynamic visited))]
                                                                             [`(if ,expr ,label-true ,label-false)
                                                                              (if (is-evaluable dynamic expr)
                                                                                  (life-static-variables program (if (try-eval env expr) label-true label-false) env dynamic visited)
                                                                                  (set))]
                                                                             [`(goto ,label) (life-static-variables program label env dynamic visited)]
                                                                             [`(return ,expr) (extract-used-statics (map car env) expr)])
                                                                           ]
                                                       ))

  ; Label generator based on life variable analyzis. This one is needed because of 'eval' inception in Futamura projections
  
  (define (label-generator-factory program dynamic)
    (λ (label env)
      (let* ([live-variable (set->list (life-static-variables program label env dynamic (set)))])
        (list label live-variable (try-eval env live-variable #f))
        ))
    )
  ;;;; LIFE VARIABLE ANALYSIS UTILITIES

; 1. Flow chart specializer written on flow chart for flow chart

; data definition:
(define division-example '((a b c) (x y z))) ;; BOTH static and dynamic should be specified
(define vs0-example '(0 1 "asdf")) ; values mapped on '(a b c) specified earlier
; Returns tail from element, or #f if not found:
;  (member 'y division-example)

(define-syntax flow-chart-mix
  (syntax-rules ()
    [(flow-chart-mix env)
  `(
    ;; program input data + initialization block
    (read program division vs0)
    (init
          (:= pp0 (caadr program))                               ; [S] get initial label
          (:= ,env (zip (car division) vs0))                     ; [D] create initial env for computations (zip (car division) and vs0)
          (:= pending-lables (set-union (set pp0) (find-blocks-in-pending (cadr division) program))) ; [S] set of labeles for 'The Trick

          ; Introducing lift variable. [L] are static by the nature, but should not apper at any state at all
          (:= label-generator
              (label-generator-factory program (cadr division))) ; [L] make closure of life-static-variables analysis
          (:= pending (set (label-generator pp0 ,env)))          ; [D] create pending set which starts from initial lable and initial static values
          
          (:= marked (set))                                      ; [D] create set of marked specialized blocks
          (:= residual '())                                      ; [D] create stub of the specialized programm (result of the mix)
          (goto while-1-cond)
          )

    ;; while pending != <empty> we have blocks 'pp' to specialize with static values from 'vs'
    (while-1-cond (if (set-empty? pending) halt while-1-body-1))
    (while-1-body-1 (:= spec-state (set-first pending))          ; [D] extract pair into 'spec-state'
                    (:= ppd (car spec-state))                    ; [D] destruct 'spec-state' and extract 'ppd'
                    (:= vs (cdr spec-state))                     ; [D] destruct 'spec-state' and extract 'vs'
                    ;; update environment: set static variables from acquired label
                    (:= ,env (updateEnv (zip (car vs) (cadr vs)) ,env))
                    (:= pending    (set-rest pending))           ; [D] and delete extracted 'spec-state' from pending set
                    (:= marked     (set-add marked spec-state))  ; [D] also add extracted pair to 'marked' ones
                    ;; convert DYNAMIC ppd to static pp (bounded static variaiton upon 'pending-lables')
                    (:= pending-lables-iter pending-lables)      ; [S] iterator upon pending lables
                    (goto while-3-cond))
    
    (while-3-cond (if (set-empty? pending-lables-iter) error-no-such-static-label while-3-body))
    (while-3-body (:= pp (set-first pending-lables-iter))
                  (:= pending-lables-iter (set-rest pending-lables-iter))
                  (if (equal? pp ppd)
                      while-1-body-2
                      while-3-cond
                  ))
    (while-1-body-2 
                    (:= bb (cdr (assoc pp program)))             ; [S] extract basic block (with goto, but without label)
                    (:= code `((,pp . ,vs)))                     ; [D] create buffer for commands 'code' starting with label
                    ;; goto inner 'while-2' block (condition section)
                    (goto while-2-cond)
                    )
    (while-1-body-3 (:= residual (append residual (list code)))  ; [D] when cycle 'while-2' is over, add generated block into residual with label 'pp'
                    ;; then move back to 'while-1' condition checking
                    (goto while-1-cond)
                    )

    ;; while bb != <empty> analyze every incoming statement and build 'code' specialized block
    (while-2-cond (if (empty? bb) while-1-body-3 while-2-body))
    (while-2-body (:= command (car bb))                          ; [S] chop next command from basic block 'bb'
                  (:= bb (cdr bb))                               ; [S] assign the rest to the basic block 'bb' itself
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
         (:= X (cadr command))            ; [S] extract a variable of assignment
         (:= expr (caddr command))        ; [S] extract an expression of assignment
         (if (member X (car division))    ; if variable in X is static by division (expr should be also static)
             while-2-body-switch-case-1-assign-static
             while-2-body-switch-case-1-assign-dynamic)
         )
        
        (while-2-body-switch-case-1-assign-static
         (:= ,env (setEnv X (try-eval ,env expr) ,env))
         (goto while-2-cond)
         )
        (while-2-body-switch-case-1-assign-dynamic
         (:= code (append code `((:= ,X ,(substitute ,env expr #t))))) ; [D]
         (goto while-2-cond)
         )

        
        ;; goto
        (while-2-body-switch-case-1-goto
         (:= next-label (cadr command))   ; [S] extract new label name
         (goto while-2-transition-compression)
         )

         ;; if
        (while-2-body-switch-case-1-if
         (:= predicate (cadr command))    ; [S] extract a branching condition
         (:= pp-true (caddr command))     ; [S] true label
         (:= pp-false (cadddr command))   ; [S] false label
         
         (if  (is-evaluable (cadr division) predicate) ; if X is static by division (!!!)
             while-2-body-switch-case-1-if-static
             while-2-body-switch-case-1-if-dynamic)
         )
        
        (while-2-body-switch-case-1-if-static
         (if  (try-eval ,env predicate)
              while-2-body-switch-case-1-if-static-true
              while-2-body-switch-case-1-if-static-false)
         )

        (while-2-body-switch-case-1-if-static-true
         (:= next-label pp-true)          ; [S]
         (goto while-2-transition-compression)
         )
        (while-2-body-switch-case-1-if-static-false
         (:= next-label pp-false)         ; [S]
         (goto while-2-transition-compression)
         )
        
        (while-2-body-switch-case-1-if-dynamic
         
         (:= label-true (label-generator pp-true ,env))         ; [D]
         (:= label-false (label-generator pp-false ,env))       ; [D]
         
         (:= pending (if (set-member? marked label-true) pending  (set-add pending label-true)))  ; [D]
         (:= pending (if (set-member? marked label-false) pending (set-add pending label-false))) ; [D]
         (:= code (append code `((if ,(substitute ,env predicate #t) ,label-true ,label-false))))
         (goto while-2-cond))

         
         ;; return
        (while-2-body-switch-case-1-return
         (:= expr (cadr command))   ; extract return statement
         (:= code (append code `((return ,(substitute ,env expr #t))))) ; add it to 'poly'
         (goto while-2-cond) ; and go back to while-2 loop
         )
         
    ;; END while-2-body-switch-case
          

          ;; input parameter: next-label
          (while-2-transition-compression
           (:= bb (cdr (assoc next-label program)))          ; extract basic block (with goto)
           (:= next-label '()) ; clear next-label when exiting function 'while-2-transition-compression'
           ; transition compression happened
           (goto while-2-cond)
           )
    
    ; program end
    (halt
     (:= header (cdar program)) ; get header of the program
     (:= header (remove* (car division) header)) ; remove static variables from the program list
     (:= header (append '(read) header)) ; add 'read' heading to the program
     (:= residual (cons header residual)) ; left the rest for the specializer
     (return residual)
     )

    ; error handlers
    (error-no-such-command (return (format "NO SUCH COMMAND ~s in block ~s" command (assoc pp program))))
    (error-no-such-static-label (return (format "NO SUCH STATIC LABEL ~s" ppd)))
    )
  ]))
  )