(module program_examples racket

  (require "int-rkt-fc.rkt")
  (require "int-fc-tur.rkt")
  (require "flow-chart-mix.rkt")
  (require "utility.rkt")
  
  (provide find_name assign_name tm-example tm-left-expander)

; Flow Chart programs

; 1. find 'name' in 'namelist' and give out corresponding value from 'valuelist'
(define find_name
  '((read name namelist valuelist)
   (search (if (equal? name (car namelist)) found cont))
   (cont (:= valuelist (cdr valuelist))
         (:= namelist (cdr namelist))
         (goto search))
   (found (return (car valuelist)))
   ))

; 2. assign 'value' to variable named 'name'
(define assign_name
  '((read name value namelist valuelist)
   (init (:= valuelist_left '())
         (goto search))
   (search (if (equal? name (car namelist)) found cont))
   (cont (:= valuelist_left (cons (car valuelist) valuelist_left))
         (:= valuelist (cdr valuelist))
         (:= namelist (cdr namelist))
         (goto search))
   (found (:= valuelist_left (cons value valuelist_left))
          (:= valuelist (cdr valuelist))
          (return (append (reverse valuelist_left) valuelist)))
   )
  )

; Turing programs

; 1. Find first '0' charactrer and change it to '1'
(define tm-example '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)))

; 2. Go to left several times and write some data
(define tm-left-expander '((0 left) (1 left) (2 left) (3 write 1) (4 left) (5 left) (6 write "#")))


)