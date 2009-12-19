;file subbytes.scm
;dependencies: state.scm byte.scm s-box-contents.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; substitution table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-substitution-table substitutions-vector)
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'substitution-table))
      ('get-substitution
        (lambda(byte)
          (vector-ref substitutions-vector (byte->dec byte))))
      ('else
        (error "make-substitution-table: unknown message: " msg)))))

;--- S Box encryption table ---

(define s-box-encrypt-table
  (make-substitution-table
   (list->vector (map hex->byte s-encrypt-hex-list))))

;--- S Box decryption table ---

(define s-box-decrypt-table
  (make-substitution-table
   (list->vector (map hex->byte s-decrypt-hex-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sub-bytes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sub-bytes state-old s-box)
  
  (define state-new (make-state))
  
  (define (iter-columns row col)
    (if (< col number-of-rows)
        (let* ((byte-old ((state-old 'get-byte) row col))
               (byte-new ((s-box 'get-substitution) byte-old)))
          ((state-new 'set-byte!) row col byte-new)
          (iter-columns row (+ col 1)))))
  
  (define (iter-rows row)
    (if (< row number-of-columns)
        (begin
          (iter-columns row 0)
          (iter-rows (+ row 1)))))
  
  (iter-rows 0)
  state-new)