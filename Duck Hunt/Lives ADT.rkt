(#%require (only racket random error))
(load "Constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Lives ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (player-life lives)

  (define (minus-life)
    (set! lives (- lives 1)))

  (define (plus-life)
    (set! lives (+ lives 1)))

  (define (reset-lives!)
    (set! lives life-normal-amount))

    (define (dispatch-lives msg)
    (cond ((eq? msg 'lives) lives)
          ((eq? msg 'minus-life) (minus-life))
          ((eq? msg 'plus-life) (plus-life))
          ((eq? msg 'reset-lives!) (reset-lives!))
          (else (error "Lives ADT - Unknown message" msg))))
  dispatch-lives)