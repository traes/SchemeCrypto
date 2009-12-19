(define (make-photon-key filters)
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'photon-key))
      ('get-filters
        (lambda() filters))
      (else
       (error "make-photon-key: unknown message: " msg)))))

(define (generate-photon-key number)
  (define (iter number acc)
    (if (= number 0)
        acc
        (iter (- number 1)
              (cons (random-filter) acc))))
  (make-photon-key (iter number '())))

(define (photon-encryption message key)
  (define message-bits 
    (message->bits message))
  (define filters 
    ((key 'get-filters)))
  (define message-encrypted 
    (bits->photon-sequence message-bits filters))
  message-encrypted)

(define (photon-decryption photon-sequence guessed-key)
  (define bits 
    (photon-sequence->bits photon-sequence ((guessed-key 'get-filters))))
  bits)

(define (select-matching-bits bits key-1 key-2)
  
  (define (matching-filters key-1 key-2)
    (define (iter filters-1 filters-2 acc)
      (cond
        ((and (null? filters-1) (null? filters-2))
         acc)
        ((or (null? filters-1) (null? filters-2))
         (error "matching-filters: different key lengths"))
        (else
         (iter (cdr filters-1)
               (cdr filters-2)
               (append acc (list (eq? (car filters-1) (car filters-2))))))))
    (iter ((key-1 'get-filters))
          ((key-2 'get-filters))
          '())) 
  
  (define (select-bits bits matches)
    (define (iter bits matches acc)
      (cond
        ((and (null? bits) (null? matches))
         acc)
        ((or (null? bits) (null? matches))
         (error "select-bits: number of bits != number of matches"))
        (else
         (iter (cdr bits)
               (cdr matches)
               (if (car matches)
                   (append acc (list (car bits)))
                   acc)))))
    (iter bits matches '()))
  
  (select-bits bits (matching-filters key-1 key-2)))
  