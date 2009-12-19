
(define (rsa-demo)

  (define message
    (begin 
      (display "=== RSA ===")
      (newline)
      (display "message (number):")
      (select-number)))

  (define min-key 100)

  (define keypair 
    (random-rsa-key-pair min-key))
  (define private-key 
    ((keypair 'get-private-key)))
  (define public-key 
    ((keypair 'get-public-key)))

  (define encrypted-message 
    (rsa-encrypt message public-key))
  (define decrypted-message 
    (rsa-decrypt encrypted-message private-key))
  
  (display "--- key ---")(newline)
  (for-each display 
          (list "public-key: " ((public-key 'get-n)) 
                " " ((public-key 'get-e))))
  (newline)
  
  (for-each display 
            (list "private-key: " ((private-key 'get-n)) 
                  " " ((private-key 'get-d))))
  (newline)
  
  (display "--- message ---")(newline)
  (for-each display (list "original message: " message))
  (newline)
  (for-each display (list "encrypted message: " encrypted-message))
  (newline)
  (for-each display (list "decrypted message : " decrypted-message))
  (newline))
