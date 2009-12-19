
(define (aes-encrypt initial-state key)
  (define expanded-key (expand-key key))
  (define number-of-rounds 11) ;for 128 bit keys
  (define result (make-state))
  
  (define (iter state round)
    (if  (< round 10)
         (let* ((after-sub-bytes
                 (sub-bytes state s-box-encrypt-table))
               (after-shift-rows
                (shift-rows-encrypt after-sub-bytes))
               (after-mix-columns
                (mix-columns-encrypt after-shift-rows))
               (after-add-round-key
                (add-round-key after-mix-columns expanded-key round)))
           (display " --- round ")(display round)(display " ---")(newline)
           (display "start of round:")(newline)
           (display-state state)(newline)
           (display "after sub-bytes:")(newline)
           (display-state after-sub-bytes)(newline)
           (display "after shift-rows:")(newline)
           (display-state after-shift-rows)(newline)
           (display "after mix-columns:")(newline)
           (display-state after-mix-columns)(newline)
           (iter after-add-round-key (+ round 1)))
         state))
               
  (display "input: ")(newline)
  (display-state initial-state)                    
  
  ;--- round 0 ---
  (set! result initial-state)
  (set! result (add-round-key result expanded-key 0))
  
  ;--- rounds 1-9 ---
  (set! result (iter result 1))
  
  ;--- round 10 ---
  
  (display "--- round 10 ---")(newline)
  (set! result (sub-bytes result s-box-encrypt-table))
  (display "after sub-bytes:")(newline)
  (display-state result)(newline)
  (set! result (shift-rows-encrypt result))
  (display "after shift-rows:")(newline)
  (display-state result)(newline)
  (set! result (add-round-key result expanded-key 10))
  (display "after add-round-key:")(newline)
  (display-state result)(newline)
  
  (display "output:")(newline)
  (display-state result)(newline)
    
  result)

(define (aes-decrypt initial-state key)
  (define expanded-key (expand-key key))
  (define number-of-rounds 11) ;for 128 bit keys
  (define result (make-state))
  
  (define (iter state round)
    (if  (> round 0)
         (let* ((after-shift-rows
                 (shift-rows-decrypt state))
	        (after-sub-bytes
                 (sub-bytes after-shift-rows s-box-decrypt-table))
                (after-add-round-key
                 (add-round-key after-sub-bytes expanded-key round))
                (after-mix-columns
                 (mix-columns-decrypt after-add-round-key)))
           (display " --- round ")(display round)(display " ---")(newline)
           (display "start of round:")(newline)
           (display-state state)(newline)
           (display "after shift-rows:")(newline)
           (display-state after-shift-rows)(newline)
           (display "after sub-bytes:")(newline)
           (display-state after-sub-bytes)(newline)
	   (display "after add-round-key")(newline)
	   (display-state after-add-round-key)(newline)
           (display "after mix-columns:")(newline)
           (display-state after-mix-columns)(newline)
           (iter after-mix-columns (- round 1)))
         state))
               
  (display "input: ")(newline)
  (display-state initial-state)                    
  
  ;--- round 11 ---
  (set! result initial-state)
  (set! result (add-round-key result expanded-key 10))
  
  ;--- rounds 9-1 ---
  (set! result (iter result 9))
  
  ;--- round 1 ---
  
  (display " --- round 1 ---")(newline)
 
  (set! result (shift-rows-decrypt result))
  (display "after shift-rows:")(newline)
  (display-state result)(newline)

  (set! result (sub-bytes result s-box-decrypt-table))
  (display "after sub-bytes:")(newline)
  (display-state result)(newline)
  
  (set! result (add-round-key result expanded-key 0))
  (display "after add-round-key:")(newline)
  (display-state result)(newline)
  
  (display "output:")(newline)
  (display-state result)(newline)
    
  result)
