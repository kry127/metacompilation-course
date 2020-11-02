#lang racket

(require "int-rkt-fc.rkt")
(require "int-fc-tur.rkt")
(require "flow-chart-mix.rkt")
(require "utility.rkt")
(require "program_examples.rkt")

; Third Futamura projection

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

