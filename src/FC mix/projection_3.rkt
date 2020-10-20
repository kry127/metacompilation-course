#lang racket

(require "int-rkt-fc.rkt")
(require "int-fc-tur.rkt")
(require "flow-chart-mix.rkt")
(require "utility.rkt")
(require "program_examples.rkt")

; Third Futamura projection
;
;   "Mix, mix, mix, mix, everybody starts to mix! Like dee dee dee, bah bah bah dah! Now you mixing with the mix."
(define mixmixmix (flow-chart-int (flow-chart-mix 'env+) `(,(flow-chart-mix 'env-aux) ; program to specialize
                                                              (
                                                               (program division pp0 pending-lables pending-lables-iter pp bb
                                                                        command X expr
                                                                        predicate pp-true pp-false next-label
                                                                        header) ; static
                                                               (ppd pending marked residual env-aux code vs spec-state
                                                                          label-true label-false) ; dynamic
                                                               )
                                                              (,(flow-chart-mix 'env) ; = program
                                                               (
                                                               (program division pp0 pending-lables pending-lables-iter pp bb
                                                                        command X expr
                                                                        predicate pp-true pp-false next-label
                                                                        header) ; static
                                                               (ppd pending marked residual env code vs spec-state
                                                                          label-true label-false) ; dynamic
                                                                ) ; = division
                                                               () () () () () () () () () () () () ()
                                                               ) ; initial values of static variables
                                                              )))

; pretty print program (returns pair of mapping of real labels to new labels + relabeled program itself)
;(println "III mapping")
;(car (flow-chart-pretty-printer mixmixmix))
(println "III program")
(cadr (flow-chart-pretty-printer mixmixmix))

; generate compiler of turing languages by feeding interpreter in mixmixmix
; launch flow-chart-mix on Turing Machine interpreter
(define mixmixmixed-TM-compiler (flow-chart-int mixmixmix `((,turing_machine ; = program
                                                               (
                                                                (program prog instruction instruction-operator expr step) ; static
                                                                (left right element) ; dynamic)
                                                                ) ; = division
                                                               () () () () () () () () () () () () ()
                                                               ) ; initial values of static variables
                                                              )))
; print out program
;(println "III compiler mapping")
;(car (flow-chart-pretty-printer mixmixmixed-TM-compiler))
(println "III compiler program")
(cadr (flow-chart-pretty-printer mixmixmixed-TM-compiler))
; try to execute partially specialized program -- we'll get a compiled code :)
(define mixmixmix-compiled-TM-example (flow-chart-int mixmixmixed-TM-compiler `((,tm-example ,tm-example () () () ()))))
mixmixmix-compiled-TM-example
(flow-chart-int mixmixmix-compiled-TM-example '((1 1 3 5 2 4 0)))


; generate mix by feeding mix in mixmixmix (we need to go deeper)
(define mixmixmix-mix (flow-chart-int mixmixmix `((,(flow-chart-mix '++env) ; program to specialize
                                                              (
                                                               (program division pp0 pending-lables pending-lables-iter pp bb
                                                                        command X expr
                                                                        predicate pp-true pp-false next-label
                                                                        header) ; static
                                                               (ppd pending marked residual ++env code vs spec-state
                                                                          label-true label-false) ; dynamic
                                                               ) ; division
                                                              () () () () () () () () () () () () ()
                                                              ))))
; we've got mix itself!!!
; pretty print program (returns pair of mapping of real labels to new labels + relabeled program itself)
;(println "III mixmixmix-mix (= id mix) mapping")
;(car (flow-chart-pretty-printer mixmixmix-mix))
(println "III mixmixmix-mix (= id mix) program")
(cadr (flow-chart-pretty-printer mixmixmix-mix))
; check that mix specializes 'tm-example' program from turing language to flow-chart language
(define mixmixmix-mixed-FC-TM (flow-chart-int mixmixmix-mix `(
                                                              (,turing_machine ; = program
                                                               (
                                                                (program prog instruction instruction-operator expr step) ; static
                                                                (left right element) ; dynamic)
                                                                ) ; = division
                                                               () () () () () () () () () () () () ()
                                                               ) ; initial values of static variables
                                                              )))

;(println "III mixmixmix-mixed-FC-TM mapping")
;(car (flow-chart-pretty-printer mixmixmix-mixed-FC-TM))
(println "III mixmixmix-mixed-FC-TM program")
(cadr (flow-chart-pretty-printer mixmixmix-mixed-FC-TM))
; compile program with this insanity :/
(define mixmixmix-mixed-compiled-tm-example (flow-chart-int mixmixmix-mixed-FC-TM `((,tm-example ,tm-example () () () ()))))
; check that compiled program makes what it does
(flow-chart-int mixmixmix-mixed-compiled-tm-example '((7 -6 8 0 915 -5 0 0 53)))
; expected: '(7 -6 8 1 915 -5 0 0 53)
