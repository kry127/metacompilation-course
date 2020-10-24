(module program_examples racket

  (require "int-rkt-fc.rkt")
  (require "int-fc-tur.rkt")
  (require "flow-chart-mix.rkt")
  (require "utility.rkt")
  
  (provide find_name assign_name tm-example tm-left-expander int-fc-fc)

  ;; Turing programs

  ; 1. Find first '0' charactrer and change it to '1'
  (define tm-example '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)))

  ; 2. Go to left several times and write some data
  (define tm-left-expander '((0 left) (1 left) (2 left) (3 write 1) (4 left) (5 left) (6 write "#")))

  ;; Flow Chart programs

  ; 1. find 'name' in 'namelist' and give out corresponding value from 'valuelist'
  (define find_name
    '((read name namelist valuelist)
     (search (if (equal? name (car namelist)) found cont))
     (cont (:= valuelist (cdr valuelist))
           (:= namelist (cdr namelist))
           (goto search))
     (found (return (car valuelist)))
     ))

  ; 2. assign 'value' to variable named 'name'
  (define assign_name
    '((read name value namelist valuelist)
     (init (:= valuelist_left '())
           (goto search))
     (search (if (equal? name (car namelist)) found cont))
     (cont (:= valuelist_left (cons (car valuelist) valuelist_left))
           (:= valuelist (cdr valuelist))
           (:= namelist (cdr namelist))
           (goto search))
     (found (:= valuelist_left (cons value valuelist_left))
            (:= valuelist (cdr valuelist))
            (return (append (reverse valuelist_left) valuelist)))
     )
    )
  ; 3. Flow Chart interpreter (self-interpreter)
  ; uses some functions in util
  (define int-fc-fc
    '((read program valuelist)                            ; [program -> S, valuelist -> D]
      (init (:= namelist (cdar program))                  ; [S] extract name of input variables of program
            (:= pending-lables (set->list (set-union (set (caadr program)) (find-blocks-in-pending #f program))))         ; [S] list of pending lables
            (:= program (cdr program))                    ; [S] reassign the rest, program -- is set of basic blocks
            (:= fc-environment (zip namelist valuelist))  ; [D] environment, in which assignments are stored
            (:= label (caar program))                     ; [D] next label of the program
            (goto exec-label))
      
      ; execute next label (input: label)
      (exec-label
            ; convert DYNAMIC label to static label-s (bounded static variaiton upon all lables)
            (:= pending-lables-iter pending-lables)    ; [S] iterator upon pending lables
            (goto exec-label-trick-cond))
    
    (exec-label-trick-cond (if (empty? pending-lables-iter) error-no-such-static-label exec-label-trick-body))
    (error-no-such-static-label (return (format "FC-FC interpreter: NO SUCH STATIC LABEL ~s" label)))
    (exec-label-trick-body (:= label-s (car pending-lables-iter))
                           (:= pending-lables-iter (cdr pending-lables-iter))
                           (if (equal? label-s label)
                             exec-label-s
                             exec-label-trick-cond
                           ))
    
      (exec-label-s (:= bb (cdr (assoc label-s program)))     ; [S] extract basic block
                    (:= label-s '())
                    (goto exec-bb))
      ; execute current commands in basic block (input: bb)
      (exec-bb (:= cmd (car bb)) ; [S]
               (:= bb (cdr bb))  ; [S]
               (goto exec-bb-switch-case-1))

      
      ; find appropriate handler for command type
      (exec-bb-switch-case-1 (if (equal? (car cmd) ':=    )     exec-bb-:= exec-bb-switch-case-2))
      (exec-bb-switch-case-2 (if (equal? (car cmd) 'if    )     exec-bb-if exec-bb-switch-case-3))
      (exec-bb-switch-case-3 (if (equal? (car cmd) 'goto  )   exec-bb-goto exec-bb-switch-case-4))
      (exec-bb-switch-case-4 (if (equal? (car cmd) 'return) exec-bb-return error-no-such-command))
      (error-no-such-command (return (format "FC-FC interpreter: NO SUCH COMMAND ~s in block ~s" cmd (assoc label-s program))))
      
      (exec-bb-:=
       (:= fc-environment (setEnv (cadr cmd) (try-eval fc-environment (caddr cmd)) fc-environment)) ; [D] update environment
       (goto exec-bb)
       )

      (exec-bb-if
         (:= label (if (try-eval fc-environment (cadr cmd)) (caddr cmd) (cadddr cmd))) ; [D] extract next label
         (goto exec-label) ; launch next computation (input parameter 'label' already set)
       )
      
      (exec-bb-goto
         (:= label-s (cadr cmd))
         (goto exec-label-s) ; launch next computation (input parameter 'label' already set)
       )

      (exec-bb-return
         (return (try-eval fc-environment (cadr cmd))) ; return the result of interpretation
       )
      
      )
    
    )
                  
)