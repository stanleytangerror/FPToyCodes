#lang scheme
(require r5rs)


(define (flat-map f lst)
  (apply append (map f lst)))

(define-syntax list-of
  (syntax-rules (<-)
    ; generate list with given exp, range, and rules
    [(_ exp (v <- alist) rule ...) 
     (flat-map (lambda (v) (list-of exp rule ...)) alist)]
    ; first rule as filter, remaining rules as rule ...
    [(_ exp filter rule ...)
     (if filter (list-of exp rule ...)
         '())]
    ; no rule
    [(_ exp) (cons exp '())]))

(define (add-one x) (+ x 1))

(list-of (list 1 2 3))

(list-of (* 2 x) (x <- '(1 2 3 4 5)) (> 3 x))

(flat-map (lambda (x) (list-of (* 2 x) (> 3 x))) '(1 2 3 4 5))

(flat-map (lambda (x) (list-of (list x y) (y <- '(a b)))) '(1 2))

(list-of (list x y) (x <- '(1 2)) (y <- '(a b)))