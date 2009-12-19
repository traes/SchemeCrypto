;file key.scm
;contents: ADT key, ADT expandend-key
;dependencies: word.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-key bytes-vector)
  (define key-length (vector-length bytes-vector))
  (define (valid-index? index)
    (and (>= index 0)
         (< index key-length)))
  (lambda(msg)
    (case msg
      ('type
        (lambda()
          'key))
      ('get-length
        (lambda()
          (* key-length 8)))
      ('get-byte
        (lambda(index)
          (if (valid-index? index)
              (vector-ref bytes-vector index)
              (error "make-key: get-byte: invalid index: " index))))
      ('get-all-bytes
        (lambda()
          (vector->list bytes-vector)))
      (else
       (error "make-key: unknown message: " msg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expanded key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-expanded-key words-vector)
  (define number-of-words (vector-length words-vector))
  (define (valid-index? index)
    (and (>= index 0)
         (< index number-of-words)))
  (lambda(msg)
    (case msg
      ('type
        (lambda() 
          'expanded-key))
      ('get-word
        (lambda(index)
          (if (valid-index? index)
              (vector-ref words-vector index)
              (error "make-expanded-key: get-word: invalid index: " index))))
      ('get-all-words
        (lambda()
          (vector->list words-vector)))
      (else
       (error "make-expanded-key: unknown message: " msg)))))

;function expand-key
;input: key
;output: expanded-key
(define (expand-key key)
  
  (define number-of-words 44) ;for 128 bit key

  (define words-vector (make-vector number-of-words))
  
  (define (set-word! index word)
    (vector-set! words-vector index word))
  
  (define (get-byte index)
    ((key 'get-byte) index))
  
  (define (byte-index->word index)
    (apply make-word 
           (map get-byte 
                (list index (+ index 1) (+ index 2) (+ index 3)))))
  
  (define (iter index)
    (if (< index number-of-words)
        (let* ((word-prev 
                (vector-ref words-vector (- index 1)))
               (word-tmp 
                (sub-word (rot-word word-prev) s-box-encrypt-table))
               (word-tmp2
                (if (= (modulo index 4) 0)
                    (xor-words word-tmp (rcon (- (floor (/ index 4)) 1)))
                    word-prev))
               (word
                (xor-words word-tmp2
                           (vector-ref words-vector (- index 4)))))
          (set-word! index word)
          (iter (+ index 1)))))
    
  ;--- the first 4 words ---
  
  (set-word! 0 (byte-index->word 0))
  (set-word! 1 (byte-index->word 4))
  (set-word! 2 (byte-index->word 8))
  (set-word! 3 (byte-index->word 12))
  
  ;--- the rest of the words ---
  
  (iter 4)
  
  (make-expanded-key words-vector))

;procedure display-expanded-key
;input: expanded-key
;output: nothing
(define (display-expanded-key expanded-key)
  (define words ((expanded-key 'get-all-words)))
  (for-each (lambda(word)
             (display-word word)
             (newline))
           words))