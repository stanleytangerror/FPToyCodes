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

;(define-syntax delay-c
;  (syntax-rules ()
;    [(_ x) (lambda () (x))]))

;(define-syntax force-c
;  (syntax-rules ()
;    [(_ x) (x)]))

;(define (delay-c val) (lambda () (val)))

;(define (force-c val) (val))

(define the-empty-stream '())

(define stream-null? null?)

(define (stream-car stream)
	(car stream))

(define (stream-cdr stream)
	(force (cdr stream)))


(define (stream-ref s n)
  (if (= n 0) (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . s)
  (if (stream-null? (car s)) the-empty-stream
      (cons-stream (apply proc (map stream-car s))
                   (apply stream-map (cons proc (map stream-cdr s))))))

(define (stream-for-each proc s)
  (if (stream-null? s) '()
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (display-stream stream)
  (stream-for-each (lambda (e) (display e) (newline)) stream))

(define (prime? x)
  (define (smallest-division x n)
    (cond ((<= x 1) 0)
          ((= (modulo x n) 0) n)
          (else (smallest-division x (+ n 1)))))
  (= (smallest-division x 2) x))

(define (list-stream . e)
    (if (null? e) the-empty-stream
        (cons-stream (car e) 
                     (apply list-stream (cdr e)))))   ; this line matters


(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (retrieve pred stream)
  (cond ((stream-null? stream) '())
        ((pred (stream-car stream)) (stream-car stream))
        (else (retrieve pred (stream-cdr stream)))))

(display-stream (stream-filter prime? (list-stream 1 2 3 4 5 6)))

(stream-cdr (cons-stream 1 2))

(retrieve (lambda (x) (> x 1000))
          (stream-map + (integers-starting-from 1) (integers-starting-from 2)))

(prime? 2)

