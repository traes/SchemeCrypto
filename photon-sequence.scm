(define (make-photon-sequence photon-list)
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'photon-sequence))
      ('null-sequence?
        (lambda() (null? photon-list)))
      ('get-current-photon
        (lambda()
          (if (null? photon-list)
              (error "make-photon-sequence: get-current-photon: null sequence")
              (let ((result (car photon-list)))
                (set! photon-list (cdr photon-list))
                result))))
      (else
       (error "make-photon-sequence: unknown message: " msg)))))

(define (bits->photon-sequence bits filters)
  (define (iter bits filters acc)
    (cond
      ((and (null? filters) (null? bits))
       acc)
      ((or (null? filters) (null? bits))
       (error "bits->photon-sequence : number of bits != number of filters"))
      (else
       (iter (cdr bits)
             (cdr filters)
             (append acc (list (make-photon (car filters) (car bits))))))))
  (make-photon-sequence (iter bits filters '())))

(define (photon-sequence->bits photon-sequence filters)
  (define (iter filters acc)
    (if (null? filters)
        acc
        (iter (cdr filters)
              (append acc
                      (list ((((photon-sequence 'get-current-photon)) 'read) 
                             (car filters)))))))
  (iter filters '()))
          
      
