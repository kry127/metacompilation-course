#lang racket

(require "int-rkt-fc.rkt")
(require "int-fc-tur.rkt")
(require "flow-chart-mix.rkt")
(require "utility.rkt")
(require "program_examples.rkt")

; Second Futamura projection on FC interpreter
(define fc-compiler (flow-chart-int (flow-chart-mix 'env+) `(,(flow-chart-mix 'env) ; program to specialize
                                                              (
                                                               (program division pp0 pending-lables pending-lables-iter pp bb
                                                                        command X expr
                                                                        predicate pp-true pp-false next-label
                                                                        header) ; static
                                                               (ppd pending marked residual env code vs spec-state
                                                                          label-true label-false) ; dynamic
                                                               )
                                                              (,int-fc-fc ; = program
                                                               (
                                                                (program namelist bb cmd
                                                                         pending-lables pending-lables-iter label-s label-true label-false) ; static
                                                                (fc-environment label valuelist) ; dynamic)
                                                                ) ; = division
                                                               () () () () () () () () () () () () ()
                                                               ) ; initial values of static variables
                                                              )))

; pretty print program (returns pair of mapping of real labels to new labels + relabeled program itself)
;(flow-chart-pretty-printer tm-compiler)
(println "The result of second projection on Flow Chart interpreter 'mix (mix int-FC-on-FC)'")
(cadr (flow-chart-pretty-printer fc-compiler)) ; but we'd like to see the result only

(println "The result of compilation of simple 'tm-example' program from Turing to FC (makes closest change 0->1):")
; check that compiler converts 'tm-example' program from turing language to flow-chart language
(define compiled-FC-TM (flow-chart-int fc-compiler `((,turing_machine () () () () () () () ()))))
(cadr (flow-chart-pretty-printer compiled-FC-TM))

; check that compiled program makes what it does
(define input '(fc interpreter works fine * * 0 * *))
(define output '(fc interpreter works fine * * 1 * *))
(println (format "Launch mixed-FC-interpreter on input: (tm-example, ~s)" input))
(println (format "Expected output: ~s" output))
(flow-chart-int compiled-FC-TM `((,tm-example ,input)))