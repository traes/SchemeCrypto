;file state.scm
;content: ADT for intern state of AES algorithm 
;dependency: byte.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define number-of-rows 4)
(define number-of-columns 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; row
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-row elements)
  
  (define (valid-index? index)
    (and (> index -1)
         (< index number-of-columns)))
  
  (define elements-vector (list->vector elements))
  
  (if (not (= (length elements) 
              number-of-columns))
      (error "make-row: invalid number of elements: " elements))
 
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'row))
      ('get-element
        (lambda(index)
          (if (valid-index? index)
              (vector-ref elements-vector index)
              (error "make-row: get-element: invalid index: " index))))
      ('get-all-elements
        (lambda()
          (vector->list elements-vector)))
      ('set-element!
        (lambda(index value)
          (if (valid-index? index)
              (vector-set! elements-vector index value)
              (error "make-row: set-element!: invalid index: " index))))
      (else
       (error "make-row: unknown message: " msg)))))

(define (valid-row-index? index)
  (and
   (> index -1)
   (< index number-of-columns)))

(define (display-row row)
  (for-each display-byte-hex ((row 'get-all-elements))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; column
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-column elements)
  
  (define (valid-index? index)
    (and (> index -1)
         (< index number-of-rows)))
  
  (define elements-vector (list->vector elements))
  
  (if (not (= (length elements) 
              number-of-rows))
      (error "make-column: invalid number of elements: " elements))
  
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'column))
      ('get-element
        (lambda(index)
          (if (valid-index? index)
              (vector-ref elements-vector index)
              (error "make-column: get-element: invalid index: " index))))
      ('get-all-elements
        (lambda()
          (vector->list elements-vector)))
      ('set-element!
        (lambda(index value)
          (if (valid-index? index)
              (vector-set! elements-vector index value)
              (error "make-column: set-element!: invalid index: " index))))
      (else
       (error "make-column: unknown message: " msg)))))     

(define (valid-column-index? index)
  (and
   (> index -1)
   (< index number-of-rows)))  

(define (display-column column)
  (for-each 
   (lambda(byte)
     (display-byte-hex byte)
     (newline))
   ((column 'get-all-elements))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-state)

  (define bytes 
    (make-vector 
     (* number-of-rows number-of-columns)))
  
  ; === aux procedures ===
  
  ; --- indices ---
  (define (state-index->vector-index row col)
    (+ (* number-of-columns row) col))
  
  (define (valid-state-index? row col)
    (and (< row number-of-rows)
         (< col number-of-columns)
         (> (* row col) -1)))
  
  ; --- row and column ---
  (define (row-elements row-index)
    (define (iter column-index acc)
      (if (>= column-index 0)
          (iter (- column-index 1)
                (cons 
                 (vector-ref bytes 
                             (state-index->vector-index row-index column-index))
                 acc))
          acc))
    (iter (- number-of-rows 1) '()))
  
  (define (column-elements column-index)
    (define (iter row-index acc)
      (if (>= row-index 0)
          (iter (- row-index 1)
                (cons 
                 (vector-ref bytes 
                             (state-index->vector-index row-index column-index))
                 acc))
          acc))
    (iter (- number-of-columns 1) '()))
  
  ; === messages ===
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'state))
      ;--- bytes ---
      ('get-byte
        (lambda(row col)
          (if (valid-state-index? row col)
              (vector-ref bytes (state-index->vector-index row col))
              (error "make-state: get-byte: invalid index"))))
      ('set-byte!
        (lambda(row col byte)
          (cond
            ((not (eq? 'byte ((byte 'type))))
             (error "make-state: set-byte!: invalid byte: " byte))
            ((not (valid-state-index? row col))
             (error "make-state: set-byte!: invalid index"))
            (else
             (vector-set! bytes (state-index->vector-index row col) byte)))))
      ;--- rows ---
      ('get-row
        (lambda(index)
          (if (valid-column-index? index)
              (make-row (row-elements index))
              (error "make-state: get-row: invalid index: " index))))
      ;--- columns ---
      ('get-column
        (lambda(index)
          (if (valid-row-index? index)
              (make-column (column-elements index))
              (error "make-state: get-column: invalid index: " index))))
      (else
       (error "make-state: unknown message: " msg)))))

(define (display-state state)
  (define (iter index)
    (if (< index number-of-columns)
        (begin
          (display-row ((state 'get-row) index))
          (newline)
          (iter (+ index 1)))))
  (iter 0))