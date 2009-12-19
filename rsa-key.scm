
;file rsa-key.scm
;dependencies: prime.scm, inverse.scm

(define (make-rsa-public-key n e)
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'rsa-public-key))
      ('get-n
        (lambda() n))
      ('get-e
        (lambda() e))
      (else
       (error "make-rsa-public-key: unknown message: " msg)))))

(define (make-rsa-private-key n d)
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'rsa-private-key))
      ('get-n
        (lambda() n))
      ('get-d
        (lambda() d))
      (else
       (error "make-rsa-private-key: unknown message: " msg)))))

(define (make-rsa-key-pair public-key private-key)
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'rsa-key-pair))
      ('get-public-key
        (lambda() public-key))
      ('get-private-key
        (lambda() private-key))
      (else
       (error "make-rsa-key-pair: unknown message: " msg)))))

(define (random-rsa-key-pair min-value)
  (define p (random-prime min-value))
  (define q (random-prime min-value))
  (define n (* p q))
  (define inverses-pair (random-inverses-pair (* (- p 1)(- q 1))))
  (define e ((inverses-pair 'get-first)))
  (define d ((inverses-pair 'get-second)))
  (make-rsa-key-pair
   (make-rsa-public-key n e)
   (make-rsa-private-key n d)))

  