#lang racket

; utility functions
(define (car-default p default)
  (if (null? p) default (car p)))

(define (cdr-default p default)
  (if (null? p) default (cdr p)))

(define (car-mt p) (car-default p " "))
(define (cdr-mt p) (cdr-default p '()))

; example of the turing program
(define program-example
'("0: if 0 goto 3"
  "1: right"
  "2: goto 0"
  "3: write 1"
))

; This function parses one line into list of plain tokens:
;       "<step>: <op> ... <op_n>"  ===> '(<step> <op_1> ... <op_n>)
(define (parse-turing-line line)
  (
   let ([splitted (string-split line ":")])
    (append
     (list (string->number (car splitted)))
     (string-split (string-trim (cadr splitted)))
     )
  )
)

;  This function converts plain text turing program to parsed version:
; hash of the form { <step> |-> <op_1> ... <op_n> }
(define (parse-turing-program program-lines)
   (let ([kv-lists
     (map parse-turing-line
          (filter (lambda (lst) (> (string-length lst) 0)) program-lines)
          )])
   (make-hash kv-lists)
   )
)

; Let us build a "virtual machine", or "interpreter" of this turing language
; We should carry:
;   - two lists (left and right tape halfs, the writing head between them)
;   - turing machine step (or state)
; Implementation is straightforward:
;   1. Take source code as <source> and right list as input to the interpreter.
;      Initialize left list as empty list.
;      Parse <source> and produce hash map which maps <step> |-> <op>
;   2. Get <op> coresponding to the current <step>. If <step> is not presented in the program, halt.
;   3. Pattern match on the "operation code", or the first word in our case
;     3.1. [left] -> just take head of left list and place it to the right list, then <step>:=<step>+1
;     3.2. [right] -> just take head of right list and place it to the left list, then <step>:=<step>+1
;     3.3. [write A] -> remove head of right list and place A instead of it, then <step>:=<step>+1
;     3.4. [goto B] -> equivalent of sequential 2.4. operators upon whole alphabet A. Override <step>:=B
;     3.5. [if A goto B] -> look at the head of the right list, if A==B, then override <step>:=B, otherwise <step>:=<step>+1
;   * If there is no head, just produce ' ' as a result of further computations
;   4. Return the result: reversed left list ++ right list
(define (turing-machine-stepper program left right step)
  (if (hash-has-key? program step)
    (match (hash-ref program step)
      ['("left")  (turing-machine-stepper program (cdr-mt left) (append (list (car-mt left)) right) (+ step 1))]
      ['("right") (turing-machine-stepper program (append (list (car-mt right)) left) (cdr-mt right) (+ step 1))]
      [`("write" ,A) (turing-machine-stepper program left (append (list A) (cdr-mt right)) (+ step 1))]
      [`("goto" ,B) (turing-machine-stepper program left right (string->number B))]
      [`("if" ,A, "goto" ,B) (if (string=? (car-mt right) A)
                               (turing-machine-stepper program left right (string->number B))
                               (turing-machine-stepper program left right (+ step 1)))]
      )
    (append (reverse left) right)
    )
)

; finally, our interpreter is:
(define (turing-machine-interpreter source input)
  (
   let* ([program (parse-turing-program source)]
        [left '()]
        [right input]
        [stepper (lambda (s) (turing-machine-stepper program left right s))])
    (stepper 0) ; begin with 0-th step
   )
  )
