;; This module provides Turing Machine interpreter written in Flow Chart with utility functions needed for proper work of int[TM]
(module int-fc-tur racket
  
  (provide turing_machine)
  ; utility function for turing machine:
  (provide car-default cdr-default car-mt cdr-mt)

  (define (car-default p default)
    (if (null? p) default (car p)))

  (define (cdr-default p default)
    (if (null? p) default (cdr p)))

  ; safe car and cdr for turing machine
  (define (car-mt p) (car-default p " "))
  (define (cdr-mt p) (cdr-default p '()))
  
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
  )
