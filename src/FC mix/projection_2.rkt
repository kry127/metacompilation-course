#lang racket

(require "int-rkt-fc.rkt")
(require "int-fc-tur.rkt")
(require "flow-chart-mix.rkt")
(require "utility.rkt")
(require "program_examples.rkt")

; Second Futamura projection
(define tm-compiler (flow-chart-int (flow-chart-mix 'env+) `(,(flow-chart-mix 'env) ; program to specialize
                                                              (
                                                               (program division pp0 pending-lables pending-lables-iter pp bb
                                                                        command X expr
                                                                        predicate pp-true pp-false next-label
                                                                        header) ; static
                                                               (ppd pending marked residual env code vs spec-state
                                                                          label-true label-false) ; dynamic
                                                               )
                                                              (,turing_machine ; = program
                                                               (
                                                                (program prog instruction instruction-operator expr step) ; static
                                                                (left right element) ; dynamic)
                                                                ) ; = division
                                                               () () () () () () () () () () () () ()
                                                               ) ; initial values of static variables
                                                              )))

; pretty print program (returns pair of mapping of real labels to new labels + relabeled program itself)
;(flow-chart-pretty-printer tm-compiler)
(println "The result of second projection on Turing Machine interpreter 'mix mix int-TM-on-FC'")
(cadr (flow-chart-pretty-printer tm-compiler)) ; but we'd like to see the result only

(println "The result of compilation of simple 'tm-example' program from Turing to FC (makes closest change 0->1):")
; check that compiler converts 'tm-example' program from turing language to flow-chart language
(define compiled-FC-TM (flow-chart-int tm-compiler `((,tm-example ,tm-example () () () ()))))
compiled-FC-TM

; check that compiled program makes what it does
(define tm-input '((8 8 99 -5 6 0 2 5 1)))
(define tm-output '(8 8 99 -5 6 1 2 5 1))
(println (format "The result of launch of program on input ~s:" tm-input))
(println (format "Expected output: ~s" tm-output))
(flow-chart-int compiled-FC-TM tm-input)