;; This module provides Flow Chart interpreter written in Racket
(module int-fc-tur racket
  
  ; define namespace
  (define-namespace-anchor a)
  (define ns (namespace-anchor->namespace a))

  (require "int-fc-tur.rkt")
  (require "flow-chart-mix.rkt")
  
  (provide flow-chart-int)

  
; util:
(define (prefix lst) (split-at lst (max (- (length lst) 1) 0)))

(define (jump-to label blocks-ast)
  (let* (
         [labeled-block (assoc label blocks-ast)]
         [block (cdr labeled-block)]
         )
    (let-values ([(assignments jump) (prefix block)])
      ;(call-with-values (lambda () (prefix block)) program-execution (prefix block) blocks-ast)
      (begin ;;; (print "jump:") (displayln label)
             (program-execution assignments (car jump) blocks-ast))
      )
  ))

(define (program-execution assignments jump blocks-ast)
  (begin
    ;map eval assignments
    (for ([assign assignments])
      (begin ;;; (print "assign:") (displayln assign)
        (match assign
        [`(:= ,var ,expr) (let ([exprval (eval expr ns)]) (eval `(define ,var ',exprval) ns))])
      ))
    (match jump
      [`(if ,expr ,label-true ,label-false) (if (eval expr ns) (jump-to label-true blocks-ast) (jump-to label-false blocks-ast))]
      [`(goto ,label) (jump-to label blocks-ast)]
      [`(return ,expr) (eval expr ns)]
      )
    ))

   
(define (flow-chart-int program-ast input)
  (match (car program-ast)
    [(list read ctx-variables ...) (begin
                                     ; first of all, assign all input variables names, specified in read section (ctx-variables list)
                                     ; the values from the input of the program
                                     ;   see issue here: https://stackoverflow.com/questions/28947041/unbound-identifier-racket-operators
                                     (eval `(define-values ,ctx-variables (apply values ',input)) ns)
                                     ; then we can step through the program
                                     ;(program-execution (prefix (cadr program-ast)) (cdr program-ast))
                                     (jump-to (caadr program-ast) (cdr program-ast))
                                     )
                                   ]
    )
  )
  )