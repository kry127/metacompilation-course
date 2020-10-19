#lang racket

(require "int-rkt-fc.rkt")
(require "int-fc-tur.rkt")
(require "flow-chart-mix.rkt")
(require "utility.rkt")
(require "program_examples.rkt")

; Third Futamura projection
(define mixmixmix (flow-chart-int (flow-chart-mix 'env+) `(,(flow-chart-mix 'env-aux) ; program to specialize
                                                              (
                                                               (program division pp0 pending-lables pending-lables-iter pp labeled-block bb
                                                                        command X expr
                                                                        predicate pp-true pp-false next-label
                                                                        header) ; static
                                                               (ppd pending marked residual env-aux code vs spec-state
                                                                          X-newval newexpr new_predicate new_vs label_true label_false new_expr new_stmt
                                                                        live-variable lval-true lvar-true lval-false lvar-false) ; dynamic
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
                                                               () () () () () () () () () () () () () ()
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
(println "III compiler mapping")
(car (flow-chart-pretty-printer mixmixmixed-TM-compiler))
(println "III compiler program")
(cadr (flow-chart-pretty-printer mixmixmixed-TM-compiler))
(println "When the body goes around")
; try to execute partially specialized program -- we'll get a compiled code :)
(define mixmixmix-compiled-TM-example (flow-chart-int mixmixmixed-TM-compiler `((,tm-example ,tm-example () () () ())) #t))
mixmixmix-compiled-TM-example
; (flow-chart-int mixmixmixed-TM-interpreter '((1 1 3 5 2 4 0)))

