(#%require (only racket random error))
(load "Position ADT.rkt")
(load "Constants.rkt")
(load "Procedures.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Bomb ADT                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-bomb position)

  (define top 300)
  (define sidestep 300)
  (define width 50)
  (define spawn-point 0)
  (define left-zero 0)
  (define alive-status #t) ;For bomb-animation
  (define animation-blocker #t) ;For bomb-animation


  (define (animation-blocker-true)
    (set! animation-blocker #t))

  (define (animation-blocker-false)
    (set! animation-blocker #f))
  

  (define (dead!)
    (set! alive-status #f))


  (define (move!) ;Find next position using function
    (let* ((bombx (position 'x))
           (bomby (position 'y)))
      (cond ((eq? alive-status #f) ((position 'y!) (+ bomby (* time 3))))
            (else((position 'x!) (+ bombx time))
                 ((position 'y!) (math-function bombx))))))


  (define (random-top)
    (set! top (random 20 300)))
  (define (random-sidestep)
    (set! sidestep (random 10 30)))
  (define (random-width)
    (set! width (random (round (/ game-x 10)) (round (/ game-x 8)))))

  (define (set-spawn-point! x)
    (set! spawn-point x))

  (define (set-left-zero! x)
    (set! left-zero x))

  (define (math-function x)
    (+ (* (/ 1 width) (square (- (- x (- spawn-point left-zero)) sidestep))) top))
  
  (define (dispatch-bomb msg)
    (cond ((eq? msg 'move!) (move!))
          ((eq? msg 'set-spawn-point!) set-spawn-point!)
          ((eq? msg 'set-left-zero!) set-left-zero!)
          ((eq? msg 'random-top) (random-top))
          ((eq? msg 'random-sidestep) (random-sidestep))
          ((eq? msg 'random-width) (random-width))
          ((eq? msg 'top) top)
          ((eq? msg 'sidestep) sidestep)
          ((eq? msg 'width) width)
          ((eq? msg 'position) position)
          ((eq? msg 'dead!) (dead!))
          ((eq? msg 'alive-status) alive-status)
          ((eq? msg 'animation-blocker) animation-blocker)
          ((eq? msg 'animation-blocker-false) (animation-blocker-false))
          ((eq? msg 'animation-blocker-true) (animation-blocker-true))
          (else (error "Bomb ADT - Unkown message" msg))))
  
  dispatch-bomb)