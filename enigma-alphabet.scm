;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alphabet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; On the original enigma devices letters were sometimes represented
; as uppercase characters and sometimes as numbers (starting at 1)

; In this source code auxilary functions are provided for both representations.
; Functions should always use the numerical representation for their arguments
; and results.
; The symbolical representation is only intended to be used in the user interface.

; For programming purposes it is sometimes more convenient to work with a 
; numbering scheme that starts at 0.
; Also for this representation are auxilary functions provided.

; Note that only characters mentioned in the alphabet can be used in Enigma messages
; ie. there is no representation for accents, numbers, and other special characters

(define alphabet '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

;index representation

(define (number->index letter)
  (- letter 1))

(define (index->number index)
  (+ index 1))

;number representation

(define (number->letter number)
  (list-ref alphabet (number->index number)))

(define (letter->number letter)
  (define (iter list teller)
    (if (null? list)
        (error "letter->number: unknown letter: " letter)
        (if (eq? letter (car list))
            teller
            (iter (cdr list) (+ teller 1)))))
  (iter alphabet 1))

(define alphabet-numbers (map letter->number alphabet))


  
