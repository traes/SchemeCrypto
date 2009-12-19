;file shiftrows.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shift-rows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (shift-rows-encrypt state-old)
  
  (define state-new (make-state))
  
  (define (copy row-old col-old row-new col-new)
    ((state-new 'set-byte!) 
     row-new col-new ((state-old 'get-byte) row-old col-old)))
  
  ;--- row 0 ---
  (copy 0 0 0 0)
  (copy 0 1 0 1)
  (copy 0 2 0 2)
  (copy 0 3 0 3)
  
  ;--- row 1 ---
  (copy 1 0 1 3)
  (copy 1 1 1 0)
  (copy 1 2 1 1)
  (copy 1 3 1 2)
  
  ;--- row 2 ---
  (copy 2 0 2 2)
  (copy 2 1 2 3)
  (copy 2 2 2 0)
  (copy 2 3 2 1)
  
  ;--- row 3 ---
  (copy 3 0 3 1)
  (copy 3 1 3 2)
  (copy 3 2 3 3)
  (copy 3 3 3 0)
  
  state-new)

(define (shift-rows-decrypt state-old)
  
  (define state-new (make-state))
  
  (define (copy row-old col-old row-new col-new)
    ((state-new 'set-byte!) 
     row-new col-new ((state-old 'get-byte) row-old col-old)))
  
  ;--- row 0 ---
  (copy 0 0 0 0)
  (copy 0 1 0 1)
  (copy 0 2 0 2)
  (copy 0 3 0 3)
  
  ;--- row 1 ---
  (copy 1 3 1 0)
  (copy 1 0 1 1)
  (copy 1 1 1 2)
  (copy 1 2 1 3)
  
  ;--- row 2 ---
  (copy 2 2 2 0)
  (copy 2 3 2 1)
  (copy 2 0 2 2)
  (copy 2 1 2 3)
  
  ;--- row 3 ---
  (copy 3 1 3 0)
  (copy 3 2 3 1)
  (copy 3 3 3 2)
  (copy 3 0 3 3)
  
  state-new)