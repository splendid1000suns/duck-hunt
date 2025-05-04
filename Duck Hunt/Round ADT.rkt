(#%require (only racket random error))
(load "Position ADT.rkt")
(load "Constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Round ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-round level roundnumber type birdamount bombamount powerupamount)

;Types = normal and hards
  
  (define round-start #t)
  (define one-timer #t)

  (define extrabirdamount 0)

  (define (powerup-decrement)
    (set! powerupamount (- powerupamount 1)))

  (if one-timer
      (begin (set! extrabirdamount birdamount)
             (set! one-timer #f)))

  (define (round-start-no!)
    (set! round-start #f))

  (define (birdamount-decrement)
    (set! birdamount (- birdamount 1)))

  (define (bombamount-decrement)
    (set! bombamount (- bombamount 1)))

  (define (double-birds)
    (set! birdamount (* birdamount 2)))

  (define (reset-rounds)
    (set! birdamount extrabirdamount))

    (define (dispatch-round msg)
    (cond ((eq? msg 'level) level)
          ((eq? msg 'roundnumber) roundnumber)
          ((eq? msg 'birdamount) birdamount)
          ((eq? msg 'round-start) round-start)
          ((eq? msg 'round-start-no!) (round-start-no!))
          ((eq? msg 'birdamount-decrement!) (birdamount-decrement))
          ((eq? msg 'bombamount) bombamount)
          ((eq? msg 'bombamount-decrement) (bombamount-decrement))
          ((eq? msg 'type) type)
          ((eq? msg 'extrabirdamount) extrabirdamount)
          ((eq? msg 'double-birds) double-birds)
          ((eq? msg 'reset-rounds) (reset-rounds))
          ((eq? msg 'powerupamount) powerupamount)
          ((eq? msg 'powerup-decrement) (powerup-decrement))
          ((eq? msg 'round-decrement!) (round-decrement!))
          (else (error "Round ADT - Unkown message" msg))))
  
  dispatch-round)