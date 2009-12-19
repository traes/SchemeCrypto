(define (make-option representation content)
  (lambda(msg)
    (case msg
      ('type
        (lambda() 'option))
      ('get-representation
        (lambda() representation))
      ('get-content
        (lambda() content))
      (else
       (error "make-option: unknown message: " msg)))))

(define (make-choice options-list)
  (define max-index (- (length options-list) 1))
  (define (iter options-list counter)
    (if (not (null? options-list))
        (let* ((option (car options-list))
               (representation ((option 'get-representation))))
          (for-each display (list counter ": " representation))
          (newline)
          (iter (cdr options-list) (+ counter 1)))))
  
  (iter options-list 0)
  (display "enter choice: ")
  (let ((number (read)))
    (if (or (not (number? number))
            (< number 0)
            (> number max-index))
        (begin
          (display "invalid choice, try again")
          (make-choice options-list))
        (let ((choice (list-ref options-list number)))
          ((choice 'get-content))))))

(define select-letter read)

(define (select-number)
  (define number (read))
  (if (number? number)
      number
      (select-number)))

(define (ascii->number ascii)
  (cond
    ((and (> ascii 64)
         (< ascii 91))
     (- ascii 94))
    ((and (> ascii 96)
          (< ascii 123))
     (- ascii 96))
    (else
     (error "ascii->number: unknown letter: " ascii))))

(define (select-string)
  (define string (read-line))
  (if (null? (string->list string))
      (select-string)
      string))

(define (select-text)
  (map ascii->number 
       (map char->integer 
            (map char-downcase
                 (string->list (select-string))))))
