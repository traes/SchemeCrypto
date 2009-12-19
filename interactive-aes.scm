
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advanced Encryption Standard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; --- message ---
(define (make-state-start-example)
  (define state-start (make-state))
  ((state-start 'set-byte!) 0 0 (hex->byte "32"))
  ((state-start 'set-byte!) 1 0 (hex->byte "43"))
  ((state-start 'set-byte!) 2 0 (hex->byte "f6"))
  ((state-start 'set-byte!) 3 0 (hex->byte "a8"))
  ((state-start 'set-byte!) 0 1 (hex->byte "88"))
  ((state-start 'set-byte!) 1 1 (hex->byte "5a"))
  ((state-start 'set-byte!) 2 1 (hex->byte "30"))
  ((state-start 'set-byte!) 3 1 (hex->byte "8d"))
  ((state-start 'set-byte!) 0 2 (hex->byte "31"))
  ((state-start 'set-byte!) 1 2 (hex->byte "31"))
  ((state-start 'set-byte!) 2 2 (hex->byte "98"))
  ((state-start 'set-byte!) 3 2 (hex->byte "a2"))
  ((state-start 'set-byte!) 0 3 (hex->byte "e0"))
  ((state-start 'set-byte!) 1 3 (hex->byte "37"))
  ((state-start 'set-byte!) 2 3 (hex->byte "07"))
  ((state-start 'set-byte!) 3 3 (hex->byte "34"))
  state-start)

(define state-start (make-state-start-example))

(define testkey 
  (make-key
   (apply vector
          (map hex->byte
               (list "2b" "7e" "15" "16" "28" "ae" "d2" "a6" "ab" "f7" "15" "88" "09" "cf" "4f" "3c")))))

(define (chop-in-groups-of-16 null-element bytes)
  (define (complete-group group)
    (if (= (length group) 16)
        group
        (complete-group (append group (list null-element)))))
  (define (iter bytes current-group groups)
    (if (null? bytes)
        (append groups (list (complete-group current-group)))
        (if (= (length current-group) 16)
            (iter bytes '() (append groups (list current-group)))
            (iter (cdr bytes) (append current-group (list (car bytes))) groups))))
  (iter bytes '() '()))

(define (flatten-groups groups-list)
  (apply append groups-list))

(define (string->groups string)
  (chop-in-groups-of-16 (hex->byte "20") (map int->byte (map char->integer (string->list string)))))

(define (group->state group)
  (define state (make-state))
  (define (iter row col bytes)
    ((state 'set-byte!) row col (car bytes))
    (if (or (< row 3)
             (< col 3))
        (if (< col 3)
            (iter row (+ col 1) (cdr bytes))
            (iter (+ row 1) 0 (cdr bytes)))
        state))
  (iter 0 0 group))

(define (state->group state)
  (define group '())
  (define (iter row col)
    (set! group (append group (list ((state 'get-byte) row col))))
    (if (or (< row 3)
             (< col 3))
        (if (< col 3)
            (iter row (+ col 1))
            (iter (+ row 1) 0)) 
        group))
  (iter 0 0)) 

(define (string->states string)  
  (map group->state (string->groups string)))

(define (states->string states)
  (list->string (map integer->char (map byte->int (flatten-groups (map state->group states))))))

;--- key ---


(define (select-password)
  ;list with 16 bytes
  (define bytes 
    (apply vector
           (caar (chop-in-groups-of-16 
                 (hex->byte "20") 
                 (string->groups 
                  (select-string))))))
  ;make key
  (make-key bytes))
    
  
(define (aes-demo)
  (define cleartext void)
  (define states-cleartext void)
  (define key void)
  (define states-encrypted void)
  (define states-decrypted void)
    
  (display "=== Advanced Encryption Standard ===")(newline)
  
  (display "message:")
  (set! cleartext (select-string))
  (set! states-cleartext (string->states cleartext))
  
  (display "password:")
  (set! key (select-password))

  (display "--- encrypting ---")(newline)
  (set! states-encrypted 
    (map (lambda(state)
           (aes-encrypt state key))
         states-cleartext))
  

  (display "--- decrypting ---")(newline)
  (set! states-decrypted 
  (map (lambda(state)
         (aes-decrypt state key))
       states-encrypted))
  
  (display "--- overview ---")(newline)
  (display "cleartext:")(newline)
  (display (states->string states-cleartext))(newline)
  (display "encrypted:")(newline)
  (display (states->string states-encrypted))(newline)
  (display "decrypted:")(newline)
  (display (states->string states-decrypted))(newline))
  

;(define s (string->states "thomasraes"))
;(states->string s)
