#lang racket

(require "utility.rkt")
(require "int-rkt-fc.rkt")
(require "int-fc-tur.rkt")
(require "flow-chart-mix.rkt")
(require "program_examples.rkt")


; first Futamura projection
; launch flow-chart-mix on Turing Machine interpreter
(define mixed-FC-interpreter (flow-chart-int (flow-chart-mix 'env) `(,int-fc-fc ; = program
                                                               (
                                                                (program namelist bb cmd
                                                                         pending-lables pending-lables-iter label-s label-true label-false) ; static
                                                                (fc-environment label valuelist) ; dynamic)
                                                                ) ; = division
                                                              (,turing_machine () () () () () () () ()) ; initial values of static variables
                                                              ) #f))


(println "The result of first projection on Flow Chart interpreter with TM interpreter 'mix int-FC-on-FC turing_machine'")
; print out program
(cadr (flow-chart-pretty-printer mixed-FC-interpreter))
; try to execute partially specialized program
(define input '(fc interpreter works fine * * 0 * *))
(define output '(fc interpreter works fine * * 1 * *))
(println (format "Launch mixed-FC-interpreter on input: (tm-example, ~s)" input))
(println (format "Expected output: ~s" output))
; check that compiled program makes what it does
(flow-chart-int mixed-FC-interpreter `((,tm-example ,input)))