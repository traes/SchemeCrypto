;file prime.scm

;function prime?
;input: integer x (> 0)
;output: boolean
(define (prime? x)
  (define (iter y)
    (cond
      ((> y (sqrt x))
       #t)
      ((= (remainder x y) 0)
       #f)
      (else
       (iter (+ y 2)))))
  (cond
    ((or (< x 1) (not (integer? x)))
     (error "prime?: invalid input: " x))
    ((= x 1)
     #f)
    ((<= x 3)
     #t)
    ((even? x)
     #f)
    (else
     (iter 3))))

;function first-n-primes
;input: integer n (>= 0)
;output: list with first n prime numbers
;comment: the resulting list is ordered,
;         the first element is the largest prime number
;         the last element is the smallest prime number
(define (first-n-primes n)
  (define (iter i acc counter)
    (if (>= counter n)
        acc
        (if (prime? i)
            (iter (+ i 1) (cons i acc) (+ counter 1))
            (iter (+ i 1) acc counter))))
  (if (or (< n 0) (not (integer? n)))
      (error "first-n-primes: invalid input:" n)
      (iter 1 '() 0)))

;function random-prime
;input integer min-value
;output prime integer (> min-value)
(define (random-prime min-value)
  (define (iter x)
    (if (prime? x)
        x
        (iter (+ x 2))))
  
  ;determine starting point of the search
  (define seed (+ min-value (random min-value)))
  (if (even? seed) (set! seed (+ seed 1)))
  
  ;search a prime
  (if (not (integer? min-value))
      (error "random-prime: invalid input: " min-value)
      (iter seed))) 