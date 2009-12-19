;file addroundkey.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-round-key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-round-key state expanded-key round-number)
  
  (define words-list 
    (map (expanded-key 'get-word)
         (list (+ (* number-of-columns round-number) 0)
               (+ (* number-of-columns round-number) 1)
               (+ (* number-of-columns round-number) 2)
               (+ (* number-of-columns round-number) 3))))
  
  (define columns-list
    (map (state 'get-column) (list 0 1 2 3)))
  
  (define new-state (make-state))
  (define (set-column! col word)
    (for-each (lambda(row)
                ((new-state 'set-byte!) row col ((word 'get-byte) row)))
              (list 0 1 2 3)))
  (define (iter col columns words)
    (if (< col number-of-columns)
        (begin
          (set-column! col 
                       (xor-words (column->word (car columns))
                                  (car words)))
          (iter (+ col 1) (cdr columns)(cdr words)))
        new-state))
  (iter 0 columns-list words-list))