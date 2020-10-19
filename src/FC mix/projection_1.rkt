#lang racket

(require "int-rkt-fc.rkt")
(require "int-fc-tur.rkt")
(require "flow-chart-mix.rkt")
(require "utility.rkt")
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
; print out program
mixed-TM-interpreter
; try to execute partially specialized program
(flow-chart-int mixed-TM-interpreter '((1 1 3 5 2 4 0)))
; expected to overwrite first 0->1, i.e.: '(1 1 3 5 2 4 1)