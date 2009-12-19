
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plug Cable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A plug cable connects 2 letters on a plugboard
; Originally it was a electrical wire which could be plugged in the plugboad

(define (make-plug-cable first-letter second-letter)
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'plug-cable))
      ('get-first-letter
        (lambda() first-letter))
      ('get-second-letter
        (lambda() second-letter))
      (else
       (error "make-plug-cable: unknown message: " msg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plug Board
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The plugboard acts as a simple monoalphabetic substitution cipher
; When the plugs of 2 letters are conneted with a plug cable, the first
; letter will be encrypted as the second (and the other way around)
; The original letter will be returned when a letter is not connected 
; to another letter

; Note that no letter should be connected with multiple cables

(define (make-plugboard plug-cables-list)
  (define (search-corresponding-letter letter)
    (define (iter cables)
      (if (null? cables)
         letter
         (let* ((cable 
                 (car cables))
                (first-letter 
                 ((cable 'get-first-letter)))
                (second-letter 
                 ((cable 'get-second-letter))))
           (cond
             ((= letter first-letter)
              second-letter)
             ((= letter second-letter)
              first-letter)
             (else
              (iter (cdr cables)))))))
    (iter plug-cables-list))
  (lambda(msg)
    (case msg
      ('type
        (lambda()
          'plugboard))
      ('encrypt
        (lambda(letter)
          (search-corresponding-letter letter)))
      (else
       (error "make-plugboard: unknown message: " msg)))))
