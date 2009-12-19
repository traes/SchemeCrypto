;file word.scm
;contents: ADT word
;dependency: byte.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-word . bytes)
  (define (valid-index? index)
    (and (>= index 0)
         (< index 4)))
  (if (not (= (length bytes) 4))
      (error "make-word: arguments are not 4 bytes: " bytes))
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'word))
      ('get-byte
        (lambda(index)
          (if (valid-index? index)
              (list-ref bytes index)
              (error "make-word: get-byte: invalid index: " index))))
      ('get-all-bytes
        (lambda()
          bytes))
      (else
       (error "make-word: unknown message: " msg)))))

;function column->word
;input: column
;output: word
(define (column->word column)
  (apply make-word ((column 'get-all-elements))))

;function rot-word
;input: word
;output: word
(define (rot-word word)
  (apply make-word
         (map (lambda(index)
                ((word 'get-byte) index))
              (list 1 2 3 0))))

;function sub-word
;input: word, substitution-table
;output: word
(define (sub-word word s-box)
  (apply make-word
         (map (lambda(index)
                ((s-box 'get-substitution) ((word 'get-byte) index)))
              (list 0 1 2 3))))

;functin xor-word
;input: 2 words
;output: word
(define (xor-words word-1 word-2)
  (define (iter bytes-1 bytes-2 acc)
    (if (null? bytes-1)
        acc
        (iter (cdr bytes-1)
              (cdr bytes-2)
              (append acc
                      (list (xor-bytes (car bytes-1)
                                       (car bytes-2)))))))
  (apply make-word (iter ((word-1 'get-all-bytes))
                         ((word-2 'get-all-bytes))
                         '())))

;procedure display-word
;input: word
;output: nothing
(define (display-word word)
  (for-each display-byte-hex ((word 'get-all-bytes))))

;vector round-constants: x^(index-1) in GF(256)
;(source: http://www.emacswiki.org/elisp/rijndael.el.gz)
(define round-constants-vector
   (vector "01"  "02"  "04"  "08"  "10"  "20"  "40"  "80"
           "1b"  "36"  "6c"  "d8"  "ab"  "4d"  "9a"  "2f"
           "5e"  "bc"  "63"  "c6"  "97"  "35"  "6a"  "d4"
           "b3"  "7d"  "fa"  "ef"  "c5"  "91"))
 
;function rcon
;input: number
;output: word
(define (rcon i)
  (make-word
   (hex->byte (vector-ref round-constants-vector i))
   (hex->byte "00")
   (hex->byte "00")
   (hex->byte "00")))
   
