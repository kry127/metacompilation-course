#lang racket

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (:= key val) (eval `(define ,key ,val) ns))
(define (prefix lst) (split-at lst (max (- (length lst) 1) 0)))

(define find_name
  '((read name namelist valuelist)
   (search (if (equal? name (car namelist)) found cont))
   (cont (:= valuelist (cdr valuelist))
         (:= namelist (cdr namelist))
         (goto search))
   (found (return (car valuelist)))
   ))

; (assq 'cont find_name)
; (eval (cadadr find_name))
;
; (define gamma (map list '(a b c) '(1 2 3)))
; (assq 'a gamma)

(define (jump-to label blocks-ast)
  (let* (
         [labeled-block (assq label blocks-ast)]
         [block (cdr labeled-block)]
         )
    (let-values ([(assignments jump) (prefix block)])
      ;(call-with-values (lambda () (prefix block)) program-execution (prefix block) blocks-ast)
      (program-execution assignments (car jump) blocks-ast)
      )
  ))

(define (program-execution assignments jump blocks-ast)
  (begin
    ;map eval assignments
    (for ([assign assignments])
      (match assign
        [`(:= ,var ,expr) (let ([exprval (eval expr ns)]) (eval `(define ,var ',exprval) ns))])
      )
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


;; tests:
(flow-chart-int find_name '(y (x y z) (1 2 3)))
;; the expected output is '2