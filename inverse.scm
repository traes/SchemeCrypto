; ADT extended-euclid-result
; contains the values d, x and y
; these values should satisfy the condition d = gcd(a,b) = ax + by
; warning: this condition is not tested during the creation of objects
(define (make-extended-euclid-result d x y)
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'extendend-euclid-result))
      ('get-d 
        (lambda() d))
      ('get-x 
        (lambda() x))
      ('get-y 
        (lambda() y))
      (else
       (error "make-extended-euclid-result: unknown message: " msg)))))

;function extended-euclid
;input: integers a,b
;output: extended-euclid-result
(define (extended-euclid a b)
  (if (= b 0)
      (make-extended-euclid-result a 1 0)
      (let* (;--- auxilariy values ---
             (aux (extended-euclid b (modulo a b)))
             (d-aux ((aux 'get-d)))
             (x-aux ((aux 'get-x)))
             (y-aux ((aux 'get-y)))
             ;--- new values ---
             (d-new d-aux)
             (x-new y-aux)
             (y-new (- x-aux (* (floor (/ a b)) y-aux))))
        (make-extended-euclid-result d-new x-new y-new))))

;function multiplicative-inverse 
;input: integers a,n
;output: integer (if the inverse exists)
;        or #f (if no inverse exists)
;exapmles: (multiplicative-inverse 28 75) -> 67
;          (multiplicative-inverse 2376 39) -> #f
(define (multiplicative-inverse a n)
  (define extended-euclid-result (extended-euclid a n))
  (define d ((extended-euclid-result 'get-d)))
  (define x ((extended-euclid-result 'get-x)))
  (if (= d 1) 
      (modulo x n)
      #f))

; ADT inverses-pair
; contains the elements first, second and n
; they should satisfy the condition:
; first * second is congruent to 1 modulo n
(define (make-inverses-pair first second n)
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'inverses-pair))
      ('get-n
        (lambda() n))
      ('get-first
        (lambda() first))
      ('get-second
        (lambda() second))
      (else
       (error "make-inverses-pair: unknown message: " msg)))))

;function random-inverses-pair
;input: integer n
;output: inverses-pair
(define (random-inverses-pair n)
  (define a (random n))
  (define b (multiplicative-inverse a n))
  (if b (make-inverses-pair a b n)
        (random-inverses-pair n)))