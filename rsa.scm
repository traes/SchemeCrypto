;file rsa.scm

(define (message->number x) x)
(define (number->message x) x)

(define (rsa-encrypt message public-key)
  (define cleartext-number (message->number message))
  (define n ((public-key 'get-n)))
  (define e ((public-key 'get-e)))
  (define encrypted-number (modulo (expt cleartext-number e) n))
  (number->message encrypted-number))

(define (rsa-decrypt message private-key)
  (define encrypted-number (message->number message))
  (define n ((private-key 'get-n)))
  (define d ((private-key 'get-d)))
  (define cleartext-number (modulo (expt encrypted-number d) n))
  (number->message cleartext-number))
