#lang racket

(require "flow-chart-mix.rkt")
(require "utility.rkt")
(require racket/sandbox)


; define namespace
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (:= key val) (eval `(define ,key ,val) ns))

; safe car and cdr for turing machine
(define (car-mt p) (car-default p " "))
(define (cdr-mt p) (cdr-default p '()))


; Flow Chart programs

; 1. find 'name' in 'namelist' and give out corresponding value from 'valuelist'
(define find_name
  '((read name namelist valuelist)
   (search (if (equal? name (car namelist)) found cont))
   (cont (:= valuelist (cdr valuelist))
         (:= namelist (cdr namelist))
         (goto search))
   (found (return (car valuelist)))
   ))

; Turing programs

; 1. Find first '0' charactrer and change it to '1'
(define tm-example '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)))

; 2. Go to left several times and write some data
(define tm-left-expander '((0 left) (1 left) (2 left) (3 write 1) (4 left) (5 left) (6 write "#")))

; Turing program interpreter written in Flow Chart (+ external symbols from Racket: car, cdr, car-mt, cdr-mt, :=, ...)
(define turing_machine
  '((read program input)
    (init (:= left '())
          (:= right input)
          (:= prog program)
          (goto exec-step))

    ;; find step block
    ;    * Input parameter: step (label of turing operator)
    ;    * output: prog (sequence of instructions)
    (find-step (:= prog program)
               (goto find-step-loop))
    (find-step-1 (:= prog (cdr prog))
                 (goto find-step-loop))
    (find-step-loop (if (equal? prog '()) error-label find-step-loop-1))
    (find-step-loop-1 (if (equal? (caar prog) step) exec-step find-step-1))
    ; find step block

    ;; next step -- just step further and eval 'exec-step'
    ;    * Input parameter: "prog" (current sequence of instructions)
    (next-step (:= prog (cdr prog))
               (goto exec-step))

    ;; step execution
    ;    * Input parameter: "prog" (current sequence of instructions)
    (exec-step (if (equal? prog '()) halt exec-step-1))
    ; check left
    (exec-step-1 (:= instruction (cdar prog))
                 (:= instruction-operator (car instruction))
                 (if (equal? instruction-operator 'left) exec-left exec-step-2))
    ; check right
    (exec-step-2 (if (equal? instruction-operator 'right) exec-right exec-step-3))
    ; check write
    (exec-step-3 (if (equal? instruction-operator 'write) exec-write exec-step-4))
    ; check goto
    (exec-step-4 (if (equal? instruction-operator 'goto) exec-goto exec-step-5))
    ; check if
    (exec-step-5 (if (equal? instruction-operator 'if) exec-if error-no-such-operator))

    (exec-left (:= element (car-mt left))
               (:= right (append (list element) right))
               (:= left (cdr-mt left))
               (goto next-step)
               )

    (exec-right (:= element (car-mt right))
                (:= left (append (list element) left))
                (:= right (cdr-mt right))
                (goto next-step)
                )

    (exec-write (:= element (cadr instruction))
                (:= right (cdr-mt right))
                (:= right (append (list element) right))
                (goto next-step)
                )

    (exec-goto (:= step (cadr instruction))
               (goto find-step)
               )

    (exec-if (:= element (car-mt right))
             (:= expr (cadr instruction))
             (:= step (cadddr instruction))
             (if (equal? expr element) find-step next-step)
             )
    ;; step execution
    
    ; program termination
    ;    * Input parameter: "left" and "right" sides of the tape
    (halt (return (append (reverse left) right)))
    
    ; error handlers
    (error-label (return "NO SUCH LABEL"))
    (error-no-such-operator (return "NO SUCH OPERATOR"))
  )
)

; (assq 'cont find_name)
; (eval (cadadr find_name))
;
; (define gamma (map list '(a b c) '(1 2 3)))
; (assq 'a gamma)

  
; Flow Chart interpreter written in Racket 

(define (jump-to label blocks-ast)
  (let* (
         [labeled-block (assoc label blocks-ast)]
         [block (cdr labeled-block)]
         )
    (let-values ([(assignments jump) (prefix block)])
      ;(call-with-values (lambda () (prefix block)) program-execution (prefix block) blocks-ast)
      (begin ;;; (print "jump:") (displayln label)
             (program-execution assignments (car jump) blocks-ast))
      )
  ))

(define (program-execution assignments jump blocks-ast)
  (begin
    ;map eval assignments
    (for ([assign assignments])
      (begin ;;; (print "assign:") (displayln assign)
        (match assign
        [`(:= ,var ,expr) (let ([exprval (eval expr ns)]) (eval `(define ,var ',exprval) ns))])
      ))
    (match jump
      [`(if ,expr ,label-true ,label-false) (if (eval expr ns) (jump-to label-true blocks-ast) (jump-to label-false blocks-ast))]
      [`(goto ,label) (jump-to label blocks-ast)]
      [`(return ,expr) (eval expr ns)]
      )
    ))

   
(define (flow-chart-int program-ast input)
  (match (car program-ast)
    [(list read ctx-variables ...) (begin
                                     ; first of all, assign all input variables names, specified in read section (ctx-variables list)
                                     ; the values from the input of the program
                                     ;   see issue here: https://stackoverflow.com/questions/28947041/unbound-identifier-racket-operators
                                     (eval `(define-values ,ctx-variables (apply values ',input)) ns)
                                     ; then we can step through the program
                                     ;(program-execution (prefix (cadr program-ast)) (cdr program-ast))
                                     (jump-to (caadr program-ast) (cdr program-ast))
                                     )
                                   ]
    )
  )

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


;; tests

; launch Flow Chart program with Racket interpreter
;(flow-chart-int find_name '(y (x y z) (1 2 3)))
; the expected output is '2

  
; launch Turing Machine with Flow Chart interpreter written on Racket
;(flow-chart-int turing_machine `(,tm-example (1 1 1 1 0 1 0 1 1 0 1)))
;(flow-chart-int turing_machine `(,tm-left-expander (1 2 3)))


; launch flow-chart-mix on 'find name' program
;(define mixed-find-name (flow-chart-int flow-chart-mix `(,find_name (name namelist) (z (x y z)))))
; print out program:
;mixed-find-name
; try to execute partially specialized program
;(flow-chart-int mixed-find-name '((1 2 3)))


; launch flow-chart-mix on Turing Machine interpreter
;(define mixed-TM-interpreter (flow-chart-int flow-chart-mix `(,turing_machine ; program to specialize
;                                                              (
;                                                               (program prog instruction instruction-operator expr step) ; static
;                                                               (left right element) ; dynamic
;                                                              ) ; division
;                                                              (,tm-example ,tm-example () () () ()) ; initial values of static variables
;                                                              )))
; print out program
;mixed-TM-interpreter
; try to execute partially specialized program
;(flow-chart-int mixed-TM-interpreter '((1 1 3 5 2 4 0)))

; second futamura projection
(define tm-compiler (flow-chart-int flow-chart-mix `(,flow-chart-mix ; program to specialize
                                                     (
                                                      (program division pp0) ; static
                                                      (pending marked residual env bb code labeled-blockresidual pp vs spec-state command
                                                               X X-newval newexpr predicate next-label new_predicate new_vs label_true label_false) ; dynamic
                                                      )
                                                     (,turing_machine ; = program
                                                      (
                                                       (program prog instruction instruction-operator expr step) ; static
                                                       (left right element) ; dynamic)
                                                       ) ; = division
                                                      () 
                                                     ) ; initial values of static variables
                                                     )))

(flow-chart-pretty-printer tm-compiler)
(flow-chart-int (cadr (flow-chart-pretty-printer tm-compiler)) `((,tm-example ,tm-example () () () ())))