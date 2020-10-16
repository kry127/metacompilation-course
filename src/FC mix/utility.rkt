; utility functions
(module interpreter-utilities racket
  (provide flow-chart-pretty-printer)
  

  ;; pretty printer for flowchart program
  (define (fc-pp-substitute-instruction mapping instruction)
    (match instruction
      [`(if ,expr ,label-true ,label-false) `(if ,expr ,(hash-ref mapping label-true) ,(hash-ref mapping label-false))]
      [`(goto ,label) `(goto ,(hash-ref mapping label))]
      [other instruction]
      ))
  (define (fc-pp-substitute-instructions mapping bb)
    (match bb
      [(cons instruction bb) (cons (fc-pp-substitute-instruction mapping instruction) (fc-pp-substitute-instructions mapping bb))]
      [instruction (fc-pp-substitute-instruction mapping instruction)]
      ))

  (define (fc-pp-substitute-block mapping bb)
    (cons (hash-ref mapping (car bb)) (fc-pp-substitute-instructions mapping (cdr bb)))
    )

  (define (fc-pp-substitute-blocks mapping blocks)
    (match blocks
      [(cons block blocks) (cons (fc-pp-substitute-block mapping block) (fc-pp-substitute-blocks mapping blocks))]
      ['() '()]))

  (define (fc-pp-substitute-program mapping program)
    (cons (car program) (fc-pp-substitute-blocks mapping (cdr program)))
    )

  (define (flow-chart-pretty-printer program-ast)
    (match program-ast
      [(cons header blocks)
       (let ([label-mapping (make-hash (map (lambda (x y) (list (car x) y)) blocks (build-list (length blocks) (lambda (x) (format "label~s" x)))))])
         (list label-mapping (fc-pp-substitute-program label-mapping program-ast)))
       ]
      )
    )
  ;; pretty printer for flowchart program (end)
  
  )