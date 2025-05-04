(#%require (only racket random error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Score ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (player-score score)

  (define (plus-score)
    (set! score (+ score regular-score)))

  (define (plus-score-pigeon)
    (set! score (+ score pigeon-score)))

  (define (plus-score-crow)
    (set! score (+ score crow-score)))

  (define (plus-score-chicken)
    (set! score (+ score chicken-score)))

  (define (plus-score-extra)
    (set! score (+ score extra-score)))

  (define (reset-score!)
    (set! score 0))
  
    (define (dispatch-score msg)
    (cond ((eq? msg 'score) score)
          ((eq? msg 'plus-score-crow) (plus-score-crow))
          ((eq? msg 'plus-score-chicken) (plus-score-chicken))
          ((eq? msg 'plus-score-pigeon) (plus-score-pigeon))
          ((eq? msg 'plus-score) (plus-score))
          ((eq? msg 'plus-score-extra) (plus-score-extra))
          ((eq? msg 'reset-score!) (reset-score!))
          (else (error "Score ADT - Unknown message" msg))))
  dispatch-score)