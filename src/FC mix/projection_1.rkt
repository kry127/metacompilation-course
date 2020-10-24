#lang racket

(require "utility.rkt")
(require "int-rkt-fc.rkt")
(require "int-fc-tur.rkt")
(require "flow-chart-mix.rkt")
(require "program_examples.rkt")


; first Futamura projection
; launch flow-chart-mix on Turing Machine interpreter
(define mixed-TM-interpreter (flow-chart-int (flow-chart-mix 'env) `(,turing_machine ; program to specialize
                                                              (
                                                               (program prog instruction instruction-operator expr step) ; static
                                                               (left right element) ; dynamic
                                                              ) ; division
                                                              (,tm-example ,tm-example () () () ()) ; initial values of static variables
                                                              ) #f))


(println "The result of first projection on Turing Machine interpreter with simple TM program 'mix int-TM-on-FC TM-example'")
; print out program
mixed-TM-interpreter
; try to execute partially specialized program
(define tm-input '((1 1 3 5 2 4 0)))
(define tm-output '(1 1 3 5 2 4 1))
(println (format "The result of launch of program on input ~s:" tm-input))
(println (format "Expected output: ~s" tm-output))
(flow-chart-int mixed-TM-interpreter tm-input)