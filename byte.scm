;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bit-1 #t)
(define bit-0 #f)

;procedure 1-or-0->bit
;input: 1 or 0
;output: bit-1 or bit-0
(define (1-or-0->bit value)
  (cond
    ((= value 1)
     bit-1)
    ((= value 0)
     bit-0)
    (else
     (error "1-or-0->bit: invalid value: " value))))

;procedure bit->1-or-0
;input: bit
;output: 1 or 0
(define (bit->1-or-0 bit)
  (if bit 1 0))

;procedure bit?
;input: object
;output: #t or #f
(define bit? boolean?)

;procedure same-bit?
;input: 2 bits
;output: #t or #f
(define (same-bit? bit-1 bit-2)
  (if bit-1 bit-2 (not bit-2)))

;procedure xor-bits
;input: 2 bits
;output: bit
(define (xor-bits bit-1 bit-2)
  (not (same-bit? bit-1 bit-2)))

;procedure display-bit
;input: bit
;output: nothing
(define (display-bit bit)
  (display (if bit 1 0)))

;function bits->dec
;input: list of bits (represented by bit-1 or bit-0)
;output: decimal number
;comment: the first element in the list has the highest exponent
;example: (bits->dec (list bit-0 bit-1 bit-1 bit-1)) -> 7
(define (bits->dec bits)
    (define (iter pwr acc bits)
      (if (null? bits)
          acc
          (iter (+ pwr 1)
                (if (car bits)
                    (+ acc (expt 2 pwr))
                    acc)
                (cdr bits))))
    (iter 0 0 (reverse bits)))

(define (random-0-or-1)
  (random 2))

(define random-bit random-0-or-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; byte
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         
;ADT byte
;input: 8 bits (represented by 1 or 0)
;output: object
;example: (make-byte bit-0 bit-0 bit-0 bit-0 bit-1 bit-1 bit-0 bit-0)
(define (make-byte . bits)
  
  (define bits-vector 
    (if (= (length bits) 8)
        (list->vector bits)
        (error "make-byte: not 8 bits: " bits)))
  
  (define (valid-index? index)
    (and (> index -1)
         (< index 8)))
   
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'byte))
      ('get-bit
        (lambda(index)
          (if (valid-index? index)
              (vector-ref bits-vector index)
              (error "make-byte: give-bit: invalid index: " index))))
      ('get-all-bits
        (lambda()
          (vector->list bits-vector)))
      ('set-bit!
        (lambda(index value)
          (cond
            ((not (valid-index? index))
             (error "make-byte: set-bit!: invalid index: " index))
            ((not (bit? value))
             (error "make-byte: set-bit!: invalid value: " value))
            (else
             (vector-set! bits-vector index value)))))
      (else
       (error "make-byte: unknown message: " msg)))))

;function bin->byte
;input: number in binary notation (8 bits)
;output: byte
;example: (make-byte 1 1 1 1 0 0 1 1)
(define (bin->byte . binary-symbols)
 (apply make-byte (map 1-or-0->bit binary-symbols))) 

;function hex->byte
;input: number in hexadecimal notation (2 characters)
;output: byte
;example: (hex->byte 0 'c)
(define (hex->byte chars)
  (define (hex->bin chars)
    (define (hex-char->bin char)
      (case char
        ('#\0 (list 0 0 0 0))('#\1 (list 0 0 0 1))('#\2 (list 0 0 1 0))
        ('#\3 (list 0 0 1 1))('#\4 (list 0 1 0 0))('#\5 (list 0 1 0 1))
        ('#\6 (list 0 1 1 0))('#\7 (list 0 1 1 1))('#\8 (list 1 0 0 0))
        ('#\9 (list 1 0 0 1))('#\a (list 1 0 1 0))('#\b (list 1 0 1 1))
        ('#\c (list 1 1 0 0))('#\d (list 1 1 0 1))('#\e (list 1 1 1 0))
        ('#\f (list 1 1 1 1))
        (else (error "hex-char->bin: no hex char: " char))))
    (append (hex-char->bin (car (string->list chars)))
            (hex-char->bin (cadr (string->list chars)))))
  (apply bin->byte (hex->bin chars)))

;function int->byte
;input: integer
;output: byte
(define (int->byte x)
  (define (iter i machten bits)
    (cond
      ((null? machten)
       bits)
      ((>= i (car machten))
       (iter (- i (car machten))
	     (cdr machten)
	     (append bits (list 1))))
      (else
	(iter i (cdr machten) (append bits (list 0))))))
  (if (or (> x 256)
	  (< x 0))
    (error "int->byte: invalid input: " x)
  (apply bin->byte (iter x (list 128 64 32 16 8 4 2 1) '()))))

(define (byte->int byte)
  (define bits ((byte 'get-all-bits)))
  (define (iter pwr acc bits)
    (if (null? bits)
        acc
        (iter (+ pwr 1)
              (if (car bits)
                  (+ acc (expt 2 pwr))
                  acc)
              (cdr bits))))
  (iter 0 0 (reverse bits)))

; function byte->dec
; input: byte
; output: decimal number
(define (byte->dec byte)
  (bits->dec ((byte 'get-all-bits))))
                   
; function same-byte?
; input: 2 bytes
; output: #t if the bytes have the same bits, otherwise #f
(define (same-byte? byte-1 byte-2)
  (define (compare-bits list-1 list-2)
    (cond
      ((null? list-1) 
       #t)
      ((not (same-bit? (car list-1) (car list-2)))
       #f)
      (else
       (compare-bits (cdr list-1) (cdr list-2)))))
  (let ((bits-1 ((byte-1 'get-all-bits)))
        (bits-2 ((byte-2 'get-all-bits))))
    (compare-bits bits-1 bits-2)))

; function xor-2-bytes
; input: 2 bytes
; output: byte
(define (xor-2-bytes byte-1 byte-2)
  (define (iter bits-1 bits-2 acc)
    (if (null? bits-1)
        (reverse acc)
        (iter (cdr bits-1)
              (cdr bits-2)
              (cons (xor-bits (car bits-1) (car bits-2))
                    acc))))
  (apply make-byte 
         (iter ((byte-1 'get-all-bits))
               ((byte-2 'get-all-bits))
               '())))

(define (xor-bytes . bytes)
  (define (iter bytes acc)
    (if (null? bytes)
        acc
        (iter (cdr bytes)
              (xor-2-bytes (car bytes) acc))))
  (iter (cdr bytes) (car bytes)))

;function left-bitshift
;input: byte
;output: byte
(define (left-bitshift byte)
  (define bits-old ((byte 'get-all-bits)))
  (define bits-new (append (cdr bits-old) (list bit-0)))
  (apply make-byte bits-new))
  
;function top-bit-set?
;input: byte
;output: #t or #f
(define (top-bit-set? byte)
  ((byte 'get-bit) 0))

; procedure display-byte-bin
; input: byte
; output: nothing
; side-effect: byte displayed in binary notation
(define (display-byte-bin byte)
  (for-each display-bit ((byte 'get-all-bits))))

; procedure display-byte-hex
; input: byte
; output: nothing
; side-effect: byte displayed in hexadecimal notation
(define (display-byte-hex byte)
  
  (define hex-characters
    (vector 0 1 2 3 4 5 6 7 8 9 'a 'b 'c 'd 'e 'f))
  
  (define (display-4-bits-hex bits)
    (display (vector-ref hex-characters (bits->dec bits))))
  
  (define (iter bits-list acc)
    (if (= (length bits-list) 4)
        (for-each display-4-bits-hex (list acc bits-list))
        (iter (cdr bits-list)
              (append acc (list (car bits-list))))))
  
  (iter ((byte 'get-all-bits)) '())
  (display " "))
          
