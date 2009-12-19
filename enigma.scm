;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rotorposition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;rotorpositions are represented as numbers

(define (next-position number) 
  (+ 1 (modulo number 26)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rotorconfiguration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A rotor can be configured by changing its position (ie which side upwards)
; and by the position of its ring
; (the rings rotates the wiring of the rotor indepent from the position)

(define (make-rotorconfiguration rotor-position ring-position)
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'rotorconfiguration))
      ('give-rotorposition
        (lambda() rotor-position))
      ('give-ring-position
        (lambda() ring-position))
      (else
       (error "make-rotorconfiguration: unknown msg:" msg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enigma key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The key of an Enigma device consists out of the following elements:
;  * the reflecter (eg. reflector model B)
;  * the used rotors and their sequence (eg. rotors I III II)
;  * the configuration of each rotor (eg. T H O)
;  * the connections of the cables (eg. R and A, E and S)

(define (make-enigma-key 
         reflector rotors-list rotorconfigurations-list plugconfigurations)
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'enigma-key))
      ('give-rotors-list
        (lambda() rotors-list))
      ('give-rotorconfigurations-list
        (lambda() rotorconfigurations-list))
      ('give-plugconfigurations
        (lambda() plugconfigurations))
      ('give-reflector
        (lambda() reflector))
      (else
       (error "make-enigma-key: unknown msg: " msg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enigma
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;wiring of an Enigma device:

;  _____    _ _ _    ___
; |     |--| | | |--|   |-- 
; |     |--| | | |--|   |--
; |  5  |--|4|3|2|--| 1 |-- 6,7,8
; |     |--| | | |--|   |--
; |_____|--|_|_|_|--|___|--
;           
; 1. plugboard
; 2. right rotor  (aka rotor 1)
; 3. middle rotor (aka rotor 2)
; 4. left rotor   (aka rotor 3)
; 5. reflector
; 6. keyboard
; 7. lamps
; 8. power source

; Note: the position of the rotor is indicated with an arabic number (eg rotor 1).
;       the wiring of a rotor is indicated with a roman number (eg rotor I).
;       (eg. rotor 1 can be rotor III)

(define (make-enigma key)
  
  ; === configuration ===
  
  ; --- rotors ---
  ;the rotors and their sequence
  (define rotors ((key 'give-rotors-list)))
  (define rotor-1 (list-ref rotors 0)) ;right rotor
  (define rotor-2 (list-ref rotors 1)) ; middle rotor
  (define rotor-3 (list-ref rotors 2)) ; left rotor
  (define reflector ((key 'give-reflector)))
  
  ;the configurations of every rotor
  (define rotorconfigurations ((key 'give-rotorconfigurations-list)))
  (define rotorconfiguration-1 (list-ref rotorconfigurations 0))
  (define rotorconfiguration-2 (list-ref rotorconfigurations 1))
  (define rotorconfiguration-3 (list-ref rotorconfigurations 2))
  
  (define rotorposition-1 ((rotorconfiguration-1 'give-rotorposition)))
  (define rotorposition-2 ((rotorconfiguration-2 'give-rotorposition)))
  (define rotorposition-3 ((rotorconfiguration-3 'give-rotorposition)))
    
  ; --- plugboard ---
  (define plugconfigurations ((key 'give-plugconfigurations)))
  (define plugboard (make-plugboard plugconfigurations))
  
  ; === encryption procedures ===
  
  ; rotor 1 always rotates with 1 positition
  ; rotor 2 rotates if rotor 1 is in the notch position
  ; rotor 3 rotates if rotor 2 is in the notch position
  ; rotor 2 also rotates if rotor 3 is in the notch position
  
  (define (rotate-rotors)
    
    ;check which rotors will rotate
    (define rotation-3? 
      (= ((rotor-2 'give-notch)) rotorposition-2))
    (define rotation-2? 
      (or (= ((rotor-1 'give-notch)) rotorposition-1)
          rotation-3?))
    
    ;rotate the rotors
    (set! rotorposition-1 (next-position rotorposition-1))
    (if rotation-2? (set! rotorposition-2 (next-position rotorposition-2)))
    (if rotation-3? (set! rotorposition-3 (next-position rotorposition-3)))
    
    ;display the current positions
    (display "rotorpositions: ")
    (for-each (lambda(number)
                (for-each display (list (number->letter number) " ")))
              (list rotorposition-3 
                    rotorposition-2 
                    rotorposition-1))
    (newline))
    
  
  ;the effect of the rotation is simulated by adding an offset to the input
  ;and output values of the rotors
  (define (enigma-encrypt letter)
    
    ;turn-forwards adds the offset
    (define (turn-forwards rotorposition letter)
     (+ 1 (modulo (+ letter rotorposition (- 2)) 26)))
    
    ;turn-backwards substracts the offsets
     (define (turn-backwards rotorposition letter)
        (define (make-positive x)
          (if (< x 1)
              (make-positive (+ x 26))
              x))
        (define tmp (- letter (- rotorposition 1)))
        (if (< tmp 1) (make-positive tmp) tmp))
    
    ;the function l->r gives the encryption of a letter
    ;when the current flows from the left to the right
    (define (l->r rotor rotorposition letter)
      (define letter-after-rotation 
        (turn-forwards rotorposition letter))
      (define encrypted-letter
        ((rotor 'encrypt) 'left-to-right letter-after-rotation))
      (define result
        (turn-backwards rotorposition encrypted-letter))
      result)
    
    ;the function r->l gives the encryption of a letter
    ;when the current flows from the right to the left
    (define (r->l rotor rotorposition letter)
      (define letter-after-rotation 
        (turn-forwards rotorposition letter))
      (define encrypted-letter
        ((rotor 'encrypt) 'right-to-left letter-after-rotation))
      (define result
        (turn-backwards rotorposition encrypted-letter))
      result)
    
    ;temporary variables:
    (let* ((t0 ((plugboard 'encrypt) letter))  ; after plugboard (1st pass)
           (t1 (r->l rotor-1 rotorposition-1 t0)) ; after 1st rotor (1st pass)
           (t2 (r->l rotor-2 rotorposition-2 t1)) ; after 2nd rotor (1st pass)
           (t3 (r->l rotor-3 rotorposition-3 t2)) ; after 3th rotor (1st pass)
           (t4 ((reflector 'encrypt) t3))         ; after reflector
           (t5 (l->r rotor-3 rotorposition-3 t4)) ; after 3th rotor (2nd pass)
           (t6 (l->r rotor-2 rotorposition-2 t5)) ; after 2nd rotor (2nd pass)
           (t7 (l->r rotor-1 rotorposition-1 t6)) ; after 1st rotor (2nd pass)
           (t8 ((plugboard 'encrypt) t7)))     ; after plugboard (2nd pass)
       t8))
  
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'enigma))
      ('encrypt
        (lambda(letter)
          (rotate-rotors)
          (enigma-encrypt letter)))
      (else
       (error "make-enigma: unknown msg: " msg)))))
    
                           
