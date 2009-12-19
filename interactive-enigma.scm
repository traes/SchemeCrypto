
(define example-enigma-key
  (make-enigma-key
   reflector-B
   (list rotor-II rotor-I rotor-III)
   (list (make-rotorconfiguration 2 10)
         (make-rotorconfiguration 7 10)
         (make-rotorconfiguration 9 10))
   (list (apply make-plug-cable (map letter->number '(E Z)))
         (apply make-plug-cable (map letter->number '(R W)))
         (apply make-plug-cable (map letter->number '(M V)))
         (apply make-plug-cable (map letter->number '(I U)))
         (apply make-plug-cable (map letter->number '(B L)))
         (apply make-plug-cable (map letter->number '(P X)))
         (apply make-plug-cable (map letter->number '(J O))))))

; rotors

(define (select-rotor)
  (make-choice (list (make-option "rotor I" rotor-I)
                     (make-option "rotor II" rotor-II)
                     (make-option "rotor III" rotor-III))))

(define (select-rotors)
  (list (begin (display "first rotor") (newline) (select-rotor))
        (begin (display "second rotor")(newline) (select-rotor))
        (begin (display "third rotor")(newline) (select-rotor))))

(define (select-reflector)
  (make-choice (list (make-option "reflector B" reflector-B)
                     (make-option "reflector C" reflector-C))))

; rotorconfigurations

(define (select-rotorconfiguration)
  (define startposition 
    (begin (display "start position: ")
           (letter->number (select-letter))))
  (define ringposition 
    (begin  (display "ring position: ")
            (letter->number (select-letter))))
  (make-rotorconfiguration startposition ringposition))
         
(define (select-rotorconfigurations)
  (list (begin (display "rotorconfiguration first rotor")
               (newline)
               (select-rotorconfiguration))
        (begin (display "rotorconfiguration second rotor")
               (newline)
               (select-rotorconfiguration))
        (begin (display "rotorconfiguration third rotor")
               (newline)
               (select-rotorconfiguration))))

; cables

(define (select-cable)
  (define first-plug 
    (begin
      (display "first plug socket:")
      (letter->number (select-letter))))
  (define second-plug 
    (begin
      (display "second plug socket:")
      (letter->number (select-letter))))
  (make-plug-cable first-plug second-plug))

(define (select-cables)
  (define (iter cables)
    (if (make-choice (list (make-option "add another cable" #t)
                           (make-option "do not add another cable" #f)))
        (iter (cons (select-cable) cables))
        cables))
  (iter '()))
                
(define (select-enigma-key)
  (make-enigma-key
   (begin (display "select reflector")
          (newline)
          (select-reflector))
   (begin (display "select rotors")
          (newline)
          (select-rotors))
   (begin (display "select rotor configurations")
          (newline)
          (select-rotorconfigurations))
   (begin (display "select plugboard cables")
          (newline)
          (select-cables))))

; encryption/decryption

(define (display-enigma-text text)
  (for-each 
   (lambda(number)
     (for-each display (list (number->letter number) " ")))
   text)
  (newline))

(define (enigma-demo)
  (define encrypted void)
  (define decrypted void)
  (define key void)
  (define cleartext void)
  (define enigma-encrypt void)
  (define enigma-decrypt void)
  
  (display " ===== ENIGMA =====")(newline)  
  (set! key (select-enigma-key))
  (display "enter message:")
  (set! cleartext (select-text))

  (set! enigma-encrypt (make-enigma key))
  (set! enigma-decrypt (make-enigma key))

  (display "----- cleartext -----")(newline)
  (display-enigma-text cleartext)
  (display "----- encryption -----")(newline)
  
  (set! encrypted (map (enigma-encrypt 'encrypt) cleartext))
  
  (display-enigma-text encrypted)
  (display "----- decryption -----")(newline)
  
  (set! decrypted (map (enigma-decrypt 'encrypt) encrypted))
  (display-enigma-text decrypted))
