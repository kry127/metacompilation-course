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
(println "The result of third projection on Turing Machine interpreter 'mix mix mix'")
(println "mix mix mix written on FC:")
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
(println "compiler-TM-to-FC = (mix mix mix) int-TM-on-FC")
(cadr (flow-chart-pretty-printer mixmixmixed-TM-compiler))
; try to execute partially specialized program -- we'll get a compiled code :)
(define mixmixmix-compiled-TM-example (flow-chart-int mixmixmixed-TM-compiler `((,tm-example ,tm-example () () () ()))))
(println "compiled-TM-example = (mix mix mix) int-TM-on-FC TM-example")
mixmixmix-compiled-TM-example

(define input '((1 1 3 5 2 4 0)))
(define output '((1 1 3 5 2 4 1)))
(println (format "Launch compiled-TM-example on input: ~s" input))
(println (format "Expected output: ~s" output))
(flow-chart-int mixmixmix-compiled-TM-example input)


(println "")
(println  "Part II. generate new mix from (mix mix mix)")
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


(println "Program of (mix mix mix) mix:")
(cadr (flow-chart-pretty-printer mixmixmix-mix))
(println "Important note: mix' = (mix mix mix) mix has one less block than mix itself!")
(println "")
(println "check that (mix mix mix) mix converts 'tm-example' interpreter to compiler")
(define mixmixmix-mixed-FC-TM (flow-chart-int mixmixmix-mix `(
                                                              (,turing_machine ; = program
                                                               (
                                                                (program prog instruction instruction-operator expr step) ; static
                                                                (left right element) ; dynamic)
                                                                ) ; = division
                                                               () () () () () () () () () () () () ()
                                                               ) ; initial values of static variables
                                                              )))

(println "compiler-TM-to-FC' = mix' int-TM-on-FC")
(cadr (flow-chart-pretty-printer mixmixmix-mixed-FC-TM))
(println "Note: that compiler-TM-to-FC' compiled with mix' = (mix mix mix) mix is alpha-equivalent to the compiler-TM-to-FC aquired by single mix")
(println "Hint: You need to rename [env \\ env++] to make them absolutely equal")
; compile program with this insanity :/
(define mixmixmix-mixed-compiled-tm-example (flow-chart-int mixmixmix-mixed-FC-TM `((,tm-example ,tm-example () () () ()))))

(println "compiled-TM-example' = mix' int-TM-on-FC TM-example")
mixmixmix-mixed-compiled-tm-example

(define input+ '((7 -6 8 0 915 -5 0 0 53)))
(define output+ '((7 -6 8 1 915 -5 0 0 53)))
(println (format "Launch compiled-TM-example' on input: ~s" input+))
(println (format "Expected output: ~s" output+))
; check that compiled program makes what it does
(flow-chart-int mixmixmix-mixed-compiled-tm-example input+)



(println "")
(println  "Part III. launch int-FC-on-FC to check it's working correctly, and then generate (mix mix mix) int-FC-on-FC to make autocompiler")


(define input++ '(8 800 555 35 35 проще позвонить чем у кого-то занимать ! 0))
(define output++ '(8 800 555 35 35 проще позвонить чем у кого-то занимать ! 1))
(println (format "Launch int-fc-fc on input: (turing_machine, (tm-example, ~s))" input++))
(println (format "Expected output: ~s" output++))
(flow-chart-int int-fc-fc `(,turing_machine (,tm-example ,input++)))

(println "We have checked, that int-fc-fc works! Now, let's feed it into (mix mix mix), we should get comp-fc-fc -- compiler from FC to FC")

(define mixmixmix-comp-fc-fc (flow-chart-int mixmixmix `((,int-fc-fc ; = program
                                                               (
                                                                (program namelist bb cmd
                                                                         pending-lables pending-lables-iter label-s label-true label-false) ; static
                                                                (fc-environment label valuelist) ; dynamic)
                                                                ) ; = division
                                                               () () () () () () () () () () () () ()
                                                               ) ; initial values of static variables
                                                              )))

(println "mixmixmix-comp-fc-fc = (mix mix mix) int-FC-on-FC")
(cadr (flow-chart-pretty-printer mixmixmix-comp-fc-fc))
; generate compiler
(define mixmixmix-comp-fc-fc-turing_machine (flow-chart-int mixmixmix-comp-fc-fc `((,turing_machine ,turing_machine () () () () () () ()))))

(println "mixmixmix-comp-fc-fc-turing_machine = (mix mix mix) int-FC-on-FC turing_machine")
;(map (λ (row) (append (cdr row) (list (car row)))) (hash->list (car (flow-chart-pretty-printer mixmixmix-comp-fc-fc-turing_machine))))
;(car (flow-chart-pretty-printer mixmixmix-comp-fc-fc-turing_machine))
(cadr (flow-chart-pretty-printer mixmixmix-comp-fc-fc-turing_machine))

(define input+++ '(final countdown 5 4 3 2 0))
(define output+++ '(final countdown 5 4 3 2 1))
(println (format "Launch mixmixmix-comp-fc-fc-turing_machine on input: (tm-example, ~s)" input+++))
(println (format "Expected output: ~s" output+++))
; check that compiled program makes what it does
(flow-chart-int mixmixmix-comp-fc-fc-turing_machine `((,tm-example ,input+++)))
(println "Works correctly, but compiler has 47 basic blocks... -_-")

