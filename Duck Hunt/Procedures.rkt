;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Procedures                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;IMPORTS;;;;;;;;;;;;;;;;;;;;;;;;;
(#%require (only racket random error))
(load "Position ADT.rkt")
(load "Constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;PROCEDURES;;;;;;;;;;;;;;;;;;;;;;;;;

;Put coordinate to the middle of the tile
(define (middlesprite sprite)
  (cons
   (/ (sprite 'get-w) 2)
   (/ (sprite 'get-h) 2)))

;Simple square procedure
(define (square p)
  (* p p))

;Make negative
(define (make-negative x)
  (- x (* 2 x)))

;Find index for element in list
(define (index-of lst element)
  (let loop ((lst lst) (index 0))
    (cond
      ((null? lst) #f) 
      ((equal? (car lst) element) index)
      (else (loop (cdr lst) (+ index 1))))))

;Give element in list given an index
(define (element-at-index lst index)
  (if (< index 0)
      #f
      (let loop ((lst lst) (i 0))
        (cond
          ((null? lst) #f) 
          ((= i index) (car lst)) 
          (else (loop (cdr lst) (+ i 1)))))))

;Remove element at index in list
(define (remove-at-index! lst index)
  (if (= index 0)
      (set! lst '())
      (let loop ((prev lst) (current (cdr lst)) (i 1))
        (if (= i index)
            (set-cdr! prev (cdr current)) ; Skip the element to remove it.
            (loop current (cdr current) (+ i 1))))))

;Find zeroes for function
(define (find-zeroes top sidestep width)
  (cons (+ sidestep (sqrt (make-negative (* top width))))
        (- sidestep (sqrt (make-negative (* top width))))))

;Find left-most-zero for function
(define (left-most-zero lst)
  (cond ((< (car lst) (cdr lst)) (car lst))
        (else (cdr lst))))                     