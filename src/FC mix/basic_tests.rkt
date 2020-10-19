#lang racket

(require "int-rkt-fc.rkt")
(require "int-fc-tur.rkt")
(require "flow-chart-mix.rkt")
(require "utility.rkt")
(require "program_examples.rkt")

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