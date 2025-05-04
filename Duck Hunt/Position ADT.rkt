(#%require (only racket random error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Position ADT                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-position x y)

  (define (square p)
    (* p p)) ;Square to calculate distance
 
  (define (x! new-x)
    (set! x new-x)) ;Set new x value
  
  (define (y! new-y)
    (set! y new-y)) ;Set new y value

  (define (position! new-position)
    (set! x (new-position 'x))
    (set! y (new-position 'y))) ;Quickly change position

  (define (in-range? other-position radius)
    (cond ((> radius (distance other-position)) #t)
          (else #f))) ;Given a radius, calulates if two positions are in-range of each other

  (define (distance other-position)
    (round (sqrt (+ (round (square (- x (other-position 'x))))
                    (round (square (- y (other-position 'y)))))))) ;Calculate distance between two points
  
  (define (dispatch-position msg)
    (cond ((eq? msg 'x) x)
          ((eq? msg 'y) y)
          ((eq? msg 'x!) x!)
          ((eq? msg 'y!) y!)
          ((eq? msg 'position!) position!)
          ((eq? msg 'distance) distance)
          ((eq? msg 'in-range?) in-range?)
          (else (error "Position ADT - Unknown message" msg))))
  dispatch-position)