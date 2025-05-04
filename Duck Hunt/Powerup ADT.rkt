(#%require (only racket random error))
(load "Position ADT.rkt")
(load "Constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Duck ADT                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-powerup position tag)

  (define active #f)

  (define timer #f)

  (define (start-timer!)
    (set! timer #t))

  (define (stop-timer!)
    (set! timer #f))

  (define (reset-timer!)
    (set! powerup-timer 0))

  (define (activate!)
    (set! active #t))

  (define (deactivate!)
    (set! active #f))

  (define powerup-timer 0)

  (define (timer-increment)
    (set! powerup-timer (+ powerup-timer 1)))


  (define (dispatch-powerup msg)
    (cond ((eq? msg 'position) position)
          ((eq? msg 'timer-increment) (timer-increment))
          ((eq? msg 'stop-timer!) (stop-timer!))
          ((eq? msg 'start-timer!) (start-timer!))
          ((eq? msg 'timer) timer)
          ((eq? msg 'powerup-timer) powerup-timer)
          ((eq? msg 'activate!) (activate!))
          ((eq? msg 'deactivate!) (deactivate!))
          ((eq? msg 'active) active)
          ((eq? msg 'tag) tag)
          ((eq? msg 'reset-timer!) (reset-timer!))
          (else (error "Powerup ADT - Unkown message" msg))))
  
  dispatch-powerup)