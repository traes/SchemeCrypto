; Thomas Raes (traes@vub.ac.be)
; Project Wiskunde voor Informatici II
; 2e kan informatica (2004-2005)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load program code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "load-code.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Demonstration of the encryption algorithms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define algorithm-options
  (list
   (make-option "Enigma" enigma-demo)
   (make-option "AES" aes-demo)
   (make-option "RSA" rsa-demo)
   (make-option "photon cryptography (simulation)" photon-demo)
   (make-option "stop demonstration" #f)))

(define (start)
  (define algorithm 
    (begin
      (display "=== Encryption Algorithms ===")
      (newline)
      (display "Select an algorithm:")
      (newline)
      (make-choice algorithm-options)))
  (if algorithm
      (begin 
        (algorithm)
        (start))))

(start)
