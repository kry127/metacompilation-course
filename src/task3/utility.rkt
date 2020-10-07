; utility functions
(module interpreter-utilities racket
  (provide prefix car-default cdr-default)
  
  (define (prefix lst) (split-at lst (max (- (length lst) 1) 0)))

  (define (car-default p default)
    (if (null? p) default (car p)))

  (define (cdr-default p default)
    (if (null? p) default (cdr p)))
  )