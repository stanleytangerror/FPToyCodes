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

(define-syntax cons-stream
  (syntax-rules ()
    [(_ x y) (cons x (delay y))]))

(define the-empty-stream '())

(define stream-null? null?)

(define (stream-car stream)
	(car stream))

(define (stream-cdr stream)
	(force (cdr stream)))

;(define (cons-stream x y)
;	(cons x (delay y)))

;; (define-values (force) (lambda (val) (val)))

;; (define-values (delay) (lambda (val) (lambda () (val))))

(define (stream-ref s n)
  (if (= n 0) (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s) the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s) 'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (prime? x)
  (define (smallest-division x n)
    (cond ((<= x 2) 0)
          ((= (modulo x n) 0) n)
          (else (smallest-division x (+ n 1)))))
  (= (smallest-division x 0) x))

(define (list-stream . e)
  (define (helper lst)
    (if (null? lst) the-empty-stream
        (cons-stream (car lst) (helper (cdr lst)))))
  (helper e))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (retrieve pred stream)
  (cond ((stream-null? stream) '())
        ((pred (stream-car stream)) (stream-car stream))
        (else (retrieve pred (stream-cdr stream)))))

(retrieve (lambda (x) (> x 1000)) (integers-starting-from 1))

;(stream-filter prime? (list-stream 1 2 3 4 5 6))
