#lang racket

(require "int-rkt-fc.rkt")
(require "int-fc-tur.rkt")
(require "flow-chart-mix.rkt")
(require "utility.rkt")


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

; Turing programs

; 1. Find first '0' charactrer and change it to '1'
(define tm-example '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)))

; 2. Go to left several times and write some data
(define tm-left-expander '((0 left) (1 left) (2 left) (3 write 1) (4 left) (5 left) (6 write "#")))


;; tests

; launch Flow Chart program with Racket interpreter
(flow-chart-int find_name '(y (x y z) (1 2 3)))
; the expected output is '2
(flow-chart-int assign_name '(y 5 (x y z) (1 2 3)))
; the expected output is '(1 5 3)
(flow-chart-int assign_name '(w 10 (x y w z w) (1 2 3 4 5)))
; the expected output is '(1 2 10 4 5)

  
; launch Turing Machine with Flow Chart interpreter written on Racket
(flow-chart-int turing_machine `(,tm-example (1 1 1 1 0 1 0 1 1 0 1)))
(flow-chart-int turing_machine `(,tm-left-expander (1 2 3)))


; launch flow-chart-mix on 'find name' program
(define mixed-find-name (flow-chart-int (flow-chart-mix 'env) `(,find_name ((name namelist) (valuelist)) (z (x y z)))))
; print out program:
mixed-find-name
; try to execute partially specialized program
(flow-chart-int mixed-find-name '((1 2 3)))


; first Futamura projection
; launch flow-chart-mix on Turing Machine interpreter
(define mixed-TM-interpreter (flow-chart-int (flow-chart-mix 'env) `(,turing_machine ; program to specialize
                                                              (
                                                               (program prog instruction instruction-operator expr step) ; static
                                                               (left right element) ; dynamic
                                                              ) ; division
                                                              (,tm-example ,tm-example () () () ()) ; initial values of static variables
                                                              )))
; print out program
mixed-TM-interpreter
; try to execute partially specialized program
(flow-chart-int mixed-TM-interpreter '((1 1 3 5 2 4 0)))


; Second Futamura projection
(define tm-compiler (flow-chart-int (flow-chart-mix 'env+) `(,(flow-chart-mix 'env) ; program to specialize
                                                              (
                                                               (program division pp0 pending-lables pending-lables-iter pp labeled-block bb
                                                                        command X expr
                                                                        predicate pp-true pp-false next-label
                                                                        header
                                                                        live-variable lval-true lvar-true lval-false lvar-false) ; static
                                                               (ppd pending marked residual env code vs spec-state
                                                                          X-newval newexpr new_predicate new_vs label_true label_false new_expr new_stmt) ; dynamic
                                                               )
                                                              (,turing_machine ; = program
                                                               (
                                                                (program prog instruction instruction-operator expr step) ; static
                                                                (left right element) ; dynamic)
                                                                ) ; = division
                                                               () () () () () () () () () () () () () () () () () () ()
                                                               ) ; initial values of static variables
                                                              )))

; pretty print program (returns pair of mapping of real labels to new labels + relabeled program itself)
(flow-chart-pretty-printer tm-compiler)
; check that compiler converts 'tm-example' program from turing language to flow-chart language
(define compiled-FC-TM (flow-chart-int tm-compiler `((,tm-example ,tm-example () () () ()))))
compiled-FC-TM
; check that compiled program makes what it does
(flow-chart-int compiled-FC-TM '((8 8 99 -5 6 0 2 5 1)))


; Third Futamura projection
(define mixmixmix (flow-chart-int (flow-chart-mix 'env+) `(,(flow-chart-mix 'env-aux) ; program to specialize
                                                              (
                                                               (program division pp0 pending-lables pending-lables-iter pp labeled-block bb
                                                                        command X expr
                                                                        predicate pp-true pp-false next-label
                                                                        header
                                                                        live-variable lval-true lvar-true lval-false lvar-false) ; static
                                                               (ppd pending marked residual env-aux code vs spec-state
                                                                          X-newval newexpr new_predicate new_vs label_true label_false new_expr new_stmt) ; dynamic
                                                               )
                                                              (,(flow-chart-mix 'env) ; = program
                                                               (
                                                               (program division pp0 pending-lables pending-lables-iter pp labeled-block bb
                                                                        command X expr
                                                                        predicate pp-true pp-false next-label
                                                                        header
                                                                        live-variable lval-true lvar-true lval-false lvar-false) ; static
                                                               (ppd pending marked residual env code vs spec-state
                                                                          X-newval newexpr new_predicate new_vs label_true label_false new_expr new_stmt) ; dynamic
                                                                ) ; = division
                                                               () () () () () () () () () () () () () () () () () () ()
                                                               ) ; initial values of static variables
                                                              )))

(println "Sing, sing, sing, sing!")
; pretty print program (returns pair of mapping of real labels to new labels + relabeled program itself)
(println "III mapping")
(car (flow-chart-pretty-printer mixmixmix))
(println "III program")
(cadr (flow-chart-pretty-printer mixmixmix))

(println "Everybody starts to sing")
; first Futamura projection with mixmixmix
; launch flow-chart-mix on Turing Machine interpreter
(define mixmixmixed-TM-compiler (flow-chart-int mixmixmix `((,turing_machine ; = program
                                                               (
                                                                (program prog instruction instruction-operator expr step) ; static
                                                                (left right element) ; dynamic)
                                                                ) ; = division
                                                               () () () () () () () () () () () () () () () () () () ()
                                                               ) ; initial values of static variables
                                                              )))
; print out program
(cadr (flow-chart-pretty-printer mixmixmixed-TM-compiler))
(println "When the body goes around")
; try to execute partially specialized program -- we'll get a compiled code :)
(define mixmixmix-compiled-TM-example (flow-chart-int mixmixmixed-TM-compiler `((,tm-example ,tm-example () () () ()))))
mixmixmix-compiled-TM-example
; (flow-chart-int mixmixmixed-TM-interpreter '((1 1 3 5 2 4 0)))

