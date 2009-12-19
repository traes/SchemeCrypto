(define filters '(rectilinear diagonal))
(define number-of-filters (length filters))
(define (random-filter)
  (list-ref filters (random number-of-filters)))

(define (make-photon filter bit)
  (define other-bit (random-bit))
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'photon))
      ('read
        (lambda(read-filter)
          (let ((result (if (equal? read-filter filter)
                            bit
                            other-bit)))
            (set! bit (random-bit))
            (set! other-bit (random-bit))
            result)))
      (else
       (error "make-photon: unknown message: " msg)))))