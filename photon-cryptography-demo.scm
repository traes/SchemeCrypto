
(define (message->bits x) x)
(define (bits->message x) x)

(define (photon-demo)
  
  (define message 
    (list 0 1 0 0 1 0 0 0 1))
  (define message-length 
    (length message))
 
  (define key-sender 
    (generate-photon-key message-length))
  (define key-receiver 
    (generate-photon-key message-length))
  
  (define photon-sequence 
    (photon-encryption message key-sender))
  (define decrypted-bits 
    (photon-decryption photon-sequence key-receiver))
  
  (define matching-bits 
    (select-matching-bits decrypted-bits key-sender key-receiver))
  
  (define (display-filters key)
    (define (iter filters number)
      (if (not (null? filters))
          (begin
            (for-each display (list number ": " (car filters)))
            (newline)
            (iter (cdr filters) (+ number 1)))))
    (iter ((key 'get-filters)) 0))
  
  (display "=== Photon Cryptography ===")(newline)
  (display "--- key sender --- ") (newline)
  (display-filters key-sender)
  (display "--- key receiver ---")(newline)
  (display-filters key-receiver)
  (display "--- bits ---")(newline)
  (for-each display (list "bits encoded by sender: " message))(newline)
  (for-each display (list "bits decoded by receiver: " decrypted-bits))(newline)
  (for-each display (list "matching bits: " matching-bits))(newline))
