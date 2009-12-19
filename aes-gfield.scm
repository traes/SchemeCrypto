;file gfield.scm
;content: procedures for Galois Field (256)
;dependencies: byte.scm state.scm

;remark: . states can be considered as GF(256) fields
;        . bytes can be considered as elements of GF(256) 
;          (ie. 8th order polynomials)

;procedure add-gf256
;input: 2 GF(256) elements (ie bytes)
;output:1 GF(256) element (ie byte)
(define (add-gf256 element-1 element-2)
  (xor-2-bytes element-1 element-2))

;procedure multiply-gf256
;input: 2 GF(256) elements (ie bytes)
;output:1 GF(256) element (ie byte)
(define (multiply-gf256 element-1 element-2)
  
  ;counter-overflow-element: (x^8 +) x^4 + x^3 + x^2 + x^0
  (define counter-overflow-element (bin->byte 0 0 0 1 1 0 1 1))
  
  (define (iter el-1-old top-bit-old? bits-2 result)
    (let* ((el-1-new 
            (if top-bit-old?
                (add-gf256 el-1-old counter-overflow-element)
                el-1-old))
           (top-bit-new?
            (top-bit-set? el-1-new)))
      (if (null? bits-2)
          result
          (iter (left-bitshift el-1-new)
                top-bit-new?
                (cdr bits-2)
                (if (car bits-2)
                    (add-gf256 el-1-new result)
                    result)))))
  
  (iter element-1 #f (reverse ((element-2 'get-all-bits))) (hex->byte "00")))
                                   
  