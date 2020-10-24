; utility functions
(module interpreter-utilities racket
  (provide flow-chart-pretty-printer)
  (provide zip envHasKey envAssoc setEnv updateEnv substitute is-evaluable try-eval extract-used-statics)
  (provide find-blocks-in-pending)
  (provide label-generator-factory)
  

  ;; pretty printer for flowchart program
  (define (fc-pp-substitute-instruction mapping instruction)
    (match instruction
      [`(if ,expr ,label-true ,label-false) `(if ,expr ,(hash-ref mapping label-true) ,(hash-ref mapping label-false))]
      [`(goto ,label) `(goto ,(hash-ref mapping label))]
      [other instruction]
      ))
  (define (fc-pp-substitute-instructions mapping bb)
    (match bb
      [(cons instruction bb) (cons (fc-pp-substitute-instruction mapping instruction) (fc-pp-substitute-instructions mapping bb))]
      [instruction (fc-pp-substitute-instruction mapping instruction)]
      ))

  (define (fc-pp-substitute-block mapping bb)
    (cons (hash-ref mapping (car bb)) (fc-pp-substitute-instructions mapping (cdr bb)))
    )

  (define (fc-pp-substitute-blocks mapping blocks)
    (match blocks
      [(cons block blocks) (cons (fc-pp-substitute-block mapping block) (fc-pp-substitute-blocks mapping blocks))]
      ['() '()]))

  (define (fc-pp-substitute-program mapping program)
    (cons (car program) (fc-pp-substitute-blocks mapping (cdr program)))
    )

  (define (flow-chart-pretty-printer program-ast)
    (match program-ast
      [(cons header blocks)
       (let ([label-mapping (make-hash (map (lambda (x y) (list (car x) y)) blocks (build-list (length blocks) (lambda (x) (format "label~s" x)))))])
         (list label-mapping (fc-pp-substitute-program label-mapping program-ast)))
       ]
      )
    )
  ;; pretty printer for flowchart program (end)

  

  ;;;; ENV UTILITIES
  (define (zip lst1 lst2) (begin ;;; (println (format "zip l1: ~s, l2: ~s" lst1 lst2))
                            (map list lst1 lst2)))
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
                                                                              (if (and dynamic (is-evaluable dynamic expr)) (set) (set label-true label-false))]
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
  (define (life-static-variables program label env dynamic [visited (set)]) (if (set-member? visited label) (set) (life-static-variables-bb program (if (assoc label program) (cdr (assoc label program)) (error "No such label" label)) env dynamic (set-add visited label))))

  (define (life-static-variables-bb program bb env dynamic visited)  (match bb
                                                       ['() (error "Empty basic block")]
                                                       [(cons command bbs) (match command
                                                                             [`(:= ,var ,expr) (set-union (extract-used-statics (map car env) expr) (life-static-variables-bb program bbs env dynamic visited))]
                                                                             [`(if ,expr ,label-true ,label-false)
                                                                              (if (and dynamic (is-evaluable dynamic expr))
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
  
  )