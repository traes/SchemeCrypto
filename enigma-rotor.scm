;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wiring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; the wiring is the core of a rotor

; it has 2 sides, on both sides there are 26 contact points
; every contact point corresponds with a letter of the alfabet
; when an electric current enters the wiring at one side
; it leaves the wiring at the other side through a contact point
; that represents another letter
; therefore it acts as a monoalphabetic substitution cipher

; this principle is illustrated using a simple example
; consisting of a wiring with 5 contact points on each side:

; A'___  _ A
;      \/
; B'_  /\_ B
;    \/
; C'_/\  _ C
;      \/
; D'_  /\_ D
;    \/
; E'_/\___ E

; the wiring diagram above can be represented as C A E B D
; this means that when the current flows from the right to
; the left through wiring, the signal corresponding with the letter A
; will exit the wiring as the signal corresponding with the letter C

; when the wiring is used in the other direction (ie. from the left to the right)
; the letter C will be changed back in an A

; the working of the above diagram is represented in the following table:
; input                 : A B C D E (i)
; output (right to left): C A E B D (ii)
; output (left to right): B D A E C (iii) 


; procedure give-inverse-wiring-numbers
; input: numerical representation of a wiring diagram and the alphabet
; ouput: numerical representation of the inverse wiring diagram
; example: (give-inverse-wiring-numbers '(3 1 5 2 4) '(1 2 3 4 5)) => '(2 4 1 5 3)

(define (give-inverse-wiring-numbers numbers alphabet-numbers)
  ;give the index of a given letter
  (define (give-index letter)
    (define (iter letters acc)
      (if (null? letters)
          (error "give-index: unknown letter: " letter)
          (if (= letter (car letters))
              acc
              (iter (cdr letters) (+ acc 1)))))
    (iter numbers 0))
  
  ;search the inverse for all the letters in the alphabet
  (define (iter unsearched-letters acc)
    (if (null? unsearched-letters)
        acc
        (iter 
         (cdr unsearched-letters)
         (append acc 
                 (list  (index->number (give-index (car unsearched-letters)))))))) 
  (iter alphabet-numbers '()))

; ADT wiring
(define (make-wiring letters)
  
  (define numbers 
    (map letter->number letters))
  
  (define inverse-numbers
    (give-inverse-wiring-numbers numbers alphabet-numbers))
  
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'wiring))
      ('encrypt-right-to-left
        (lambda(number)
          (list-ref numbers (number->index number))))
      ('encrypt-left-to-right
        (lambda(number)
          (list-ref inverse-numbers (number->index number))))
      (else
       (error "make-wiring: unknown msg: " msg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rotor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A rotor encrypts letters using a wiring.
; Because is rotates it is a polyalphabetic substitution cipher
; (a simple wiring is a monoalfabetic substitution cipher)

; Every rotor has a notch, the position of this notch determines
; when the rotor on the left side rotates.
; The position of this notch is represented with a letter or number.

;                  _
;                 |_|
;                 |_|
;                 |_|
;           _ ____|_|
;          | |    |_|       
;         .| | V  |_|.        
;          | |    |_|        
; Notch-->.|=| U  |_|.             
;          | |    |_|             
;         .| | T  |_|. <-- Contact point for wiring
;     _____| |    |_|____________              
;    /____.| | S  |_|.__________/   <-- Axis      
;          | |    |_|                
;         .| | R  |_|.                
;          | |    |_|                              
;         .| | Q  |_|.                 
;          | |    |_|                
;         .| | P  |_|.                
;          |_|____|_|     
;                 |_|
;                 |_|
;                 |_|
;                 |_|
;                      



(define (make-rotor wiring notch)
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'rotor))
      ('give-notch
        (lambda() notch))
      ('encrypt
        (lambda(direction letter)
          (case direction
            ('right-to-left
              ((wiring 'encrypt-right-to-left) letter))
            ('left-to-right
              ((wiring 'encrypt-left-to-right) letter))
            (else
             (error "make-rotor: encrypt: unknown direction: " direction)))))
      (else
       (error "make-rotor: unknown msg: " msg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reflector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The reflector is a special kind of rotor.

; It doesn't rotate but sends the current back.
; This requires a different wiring.
; The wiring of a reflector can be represented using letterpairs,
; this are letters that are connected, ie. the first letter will
; be encrypted as the second, and the second as the first

; Therefore a reflector (and a Enigma device in general) is unable
; to encrypt a letter as itself.

; example (with only 4 letters):
;   _____
;  |  ___|_ A
;  | |   |
;  | |  _|_ B
;  | | | |
;  | |_|_|_ C
;  |   | |
;  |   |_|_ D
;  |_____| 
;  

(define (make-letterpair first second)
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'letterpair))
      ('give-first
        (lambda() first))
      ('give-second
        (lambda() second))
      (else
       (error "make-letterpair: unknown msg: " msg)))))

(define (make-reflector letterpairs-list)
  (define (encrypt letter)
    (define (iter letterpairs)
      (if (null? letterpairs)
          (error "make-reflector: encrypt: unknown letter: " letter)
          (let* ((letterpair (car letterpairs))
                 (first ((letterpair 'give-first)))
                 (second ((letterpair 'give-second))))
            (cond
              ((= first letter)
               second)
              ((= second letter)
               first)
              (else
               (iter (cdr letterpairs)))))))
    (iter letterpairs-list))
  
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'reflector))
      ('encrypt
        (lambda(letter)
          (encrypt letter)))
      (else
       (error "make-reflector: unknown msg: " msg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Example rotors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;source: http://www.ellsbury.com/ultraenigmawiring.htm

;rotor I
(define wiring-I 
  (make-wiring '(E K M F L G D Q V Z N T O W Y H X U S P A I B R C J)))
(define notch-I (letter->number 'Q))
(define rotor-I (make-rotor wiring-I notch-I))

;rotor II
(define wiring-II 
  (make-wiring '(A J D K S I R U X B L H W T M C Q G Z N P Y F V O E)))
(define notch-II (letter->number 'E))
(define rotor-II (make-rotor wiring-II notch-II))
  
;rotor III
(define wiring-III 
  (make-wiring '(B D F H J L C P R T X V Z N Y E I W G A K M U S Q O)))
(define notch-III (letter->number 'V))
(define rotor-III (make-rotor wiring-III notch-III))

;reflector B
(define letterpairs-reflector-B
  (map (lambda(letter-list)
         (let ((first (letter->number (car letter-list)))
               (second (letter->number (cadr letter-list))))
           (make-letterpair first second)))
       (list '(A Y) '(B R) '(C U) '(D H) '(E Q)
             '(F S) '(G L) '(I P) '(J X) '(K N) 
             '(M O) '(T Z) '(V W))))
(define reflector-B (make-reflector letterpairs-reflector-B))

;reflector C
(define letterpairs-reflector-C
  (map (lambda(letter-list)
         (let ((first (letter->number (car letter-list)))
               (second (letter->number (cadr letter-list))))
           (make-letterpair first second)))
       (list '(A F) '(B V) '(C P) '(D J) '(E I)
             '(G O) '(H Y) '(K R) '(L Z) '(M X) 
             '(N W) '(Q T) '(S U))))
(define reflector-C (make-reflector letterpairs-reflector-C))