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

(define (memo-proc proc)
  (let ((already-run? #f)
        (result #f))
    (lambda ()
      (if already-run? result
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)))))

(define-syntax-rule (delay-c proc) 
    (memo-proc (lambda () proc)))

(define-syntax-rule (force-c delayed-object)
  (delayed-object))

(define-syntax cons-stream
  (syntax-rules ()
    [(_ x y) (cons x (delay-c y))]))

(define the-empty-stream '())

(define stream-null? null?)

(define (stream-car stream)
	(car stream))

(define (stream-cdr stream)
	(force-c (cdr stream)))


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
  (stream-for-each (lambda (e) (display e) (display " ")) stream))

(define (list-stream . e)
    (if (null? e) the-empty-stream
        (cons-stream (car e) 
                     (apply list-stream (cdr e)))))   ; this line matters

(define (sub-stream stream from to)
  (define (helper stream n)
    (cond [(and (>= n from) (< n to))
           (cons-stream (stream-car stream)
                        (helper (stream-cdr stream) (+ n 1)))]
          [(< n to) (helper (stream-cdr stream) (+ n 1))]
          [else '()]))
  (helper stream 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stream-prf init next)
  (define seq
    (cons-stream init (stream-map next seq)))
  seq)

(define (prime? x)
  (define (smallest-division x n)
    (cond ((<= x 1) 0)
          ((= (modulo x n) 0) n)
          (else (smallest-division x (+ n 1)))))
  (= (smallest-division x 2) x))

(display-stream (stream-filter prime? (list-stream 1 2 3 4 5 6)))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (retrieve pred stream)
  (cond ((stream-null? stream) '())
        ((pred (stream-car stream)) (stream-car stream))
        (else (retrieve pred (stream-cdr stream)))))

(stream-cdr (cons-stream 1 2))

(retrieve (lambda (x) (> x 1000))
          (stream-map + (integers-starting-from 1) (integers-starting-from 2)))

(prime? 2)

(display-stream (sub-stream (integers-starting-from 20) 10 20))

(define (sieve stream)
  (cons-stream (stream-car stream)
               (sieve (stream-filter
                       (lambda (x)
                         (not (= (modulo x (stream-car stream)) 0)))
                       (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(display-stream (sub-stream primes 0 10))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (minus-streams s1 s2)
  (stream-map - s1 s2))

(define (multi-streams s1 s2)
  (stream-map * s1 s2))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* factor x)) s))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define ones (cons-stream 1 ones))

(define (nums n)
  (define seq (cons-stream n seq))
  seq)

(display-stream (sub-stream (nums 1.5) 0 10))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(display-stream (sub-stream integers 1 20))

(define fibs
  (cons-stream 0 (cons-stream 1
                              (add-streams fibs (stream-cdr fibs)))))

(display-stream (sub-stream fibs 10 20))

(define s (cons-stream 1 (add-streams s s)))

(display-stream (sub-stream s 0 10))
  
(define factorials
  (cons-stream 1 (multi-streams (stream-cdr integers) factorials)))

(display-stream (sub-stream factorials 0 10))

;;;;;;;;;;;;;;;;;;;;;;;;

(define (convergence x depth step)
  (define guesses
    (cons-stream x (stream-map step guesses)))
  (display-stream (sub-stream guesses 0 depth)))

(define (sqrt-improve root depth) (convergence root depth (lambda (x) (/ (+ x (/ root x)) 2))))

(sqrt-improve 2 5)

(define (pi precise)
  (define pi-terms
    (div-streams (stream-prf 1 (lambda (x) (* x -1))) (minus-streams (multi-streams (nums 2) integers) ones)))
  (define pi-sums
    (cons-stream 1 (add-streams (stream-cdr pi-terms) pi-sums)))
  (* 4 (stream-ref pi-sums precise)))

(pi 10)

;;;;;;;;;;;;;;;;;;;

(define (pairs s t)
  (define (interleave s1 s2)                    ; get each element from s and t
    (if (stream-null? s1) s2
        (cons-stream (stream-car s1)
                     (interleave s2 (stream-cdr s1)))))
  (cons-stream
   (list (stream-car s) (stream-car t))         ; pair(s[0], t[0])
   (interleave                                  ; inter-iterate through pairs1, pairs2
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))                 ; pairs1: pairs of s[0], t[1:]
    (pairs (stream-cdr s) (stream-cdr t)))))    ; pairs2: pairs of s[1:], t[1:]

(display-stream 
 (sub-stream 
  (pairs (stream-prf 1 (lambda (x) (+ x 2)))
         (stream-prf 2 (lambda (x) (+ x 2))))
  0 1000))

;;;;;;;;;;;;;;;;;;;;;

(define (integrel integrand initial-value dt)
  (define sums
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                             sums)))
  sums)

(display-stream (sub-stream (integrel (nums 10) 0 1) 0 10))

