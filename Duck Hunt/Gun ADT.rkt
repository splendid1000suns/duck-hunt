(#%require (only racket random error))
(load "Position ADT.rkt")
(load "Constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Gun ADT                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-gun position)

  (define shot? #f)

  ((window 'set-mouse-move-callback!)
   (lambda (x y)
     (let ((mouse (cons x y)))
       ((position 'x!) (car mouse))
       ((position 'y!) (cdr mouse)))))

  ((window 'set-mouse-click-callback!)
   (lambda (mouse-button status x y)
     (cond ((and (eq? mouse-button 'left)
                 (eq? status 'pressed)
                 reload)
            (set! shot? #t)
            (set! click-timer 0)
            (if (eq? current-gun 1) (set! reload-timer 0)))
           (else (set! shot? #f)))))


(define (dispatch-gun msg)
  (cond ((eq? msg 'position) position)
        ((eq? msg 'shot?) shot?)
        (else (error "Gun ADT - Unkown message" msg))))
  
dispatch-gun)