(#%require (only racket random error))
(load "Position ADT.rkt")
(load "Constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Duck ADT                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-duck position tag)

  (define removed #f)

  (define (removed!)
    (set! removed #t))

  (define water-closer #t)

  (define duck-velocity 1.5)
  (define pigeon-velocity 1)
  (define crow-velocity 2)

  (define caught-in-net? #f)

  (define (make-slow)
    (if (and watered water-closer)
        (begin (set! duck-velocity (speed-decrementer duck-velocity))
               (set! pigeon-velocity (speed-decrementer pigeon-velocity))
               (set! crow-velocity (speed-decrementer crow-velocity))
               (set! water-closer #f))))

  (define watered #f)

  (define (net-yes)
    (set! caught-in-net? #t))

  (define (net-no)
    (set! caught-in-net? #f))

  (define (water-yes)
    (set! watered #t))

  (define (water-no)
    (set! watered #f))

  (define wall-touching? #f)
  (define current-wall 0)
  (define wall-counter 0)
  (define angle (random 180 360))
  (define alive-status #t) ;For dead-animation
  (define animation-blocker #t) ;For dead-animation
  (define obstacle-status #f) ;For obstacle animation
  (define lives 0) ;Lives
  (define obstacle-randomiser (random 0 3))
  (define score-stopper #t)

  ;PIGEON;
  (define pigeon-on-ground #f) ;Pigeon on ground or not
  (define pigeon-timer 0) ;Pigeon timer
  (define allow-timer #f) ;Start & stop timer
  (define first-wall-touched #f) ;Goes true if first wall touched
  (define pigeon-closer #t) ;Uses same logic as obstacle animation to close off conditional branch after activating
  (define pigeon-open-timer 0)
  (define allow-open-timer #f)
  (define bird-randomiser (random 0 2))
  (define bird-randomiser1 (random 0 2))

  ;CROW
  (define crow-timer 0)
  (define allow-crow-timer #f)
  (define crow-closer #t)
  (define crow-open-timer 0)
  (define allow-crow-open-timer #f)
  (define crow-randomiser (random 0 3))
  (define crow-randomiser1 (random 0 2))

  (define (speed-decrementer constant)
    (/ constant 1.3))
  
  (cond ((eq? tag 'duck) (set! lives 1))
        ((eq? tag 'chicken) (set! lives 1))
        ((eq? tag 'crow) (set! lives 3))
        ((eq? tag 'pigeon) (set! lives 2)))

  (define (time-incrementer time)
    (set! time (+ time 1)))

  (define (pigeon-timer-increment)
    (set! pigeon-timer (+ pigeon-timer 1)))

  (define (pigeon-open-timer-increment)
    (set! pigeon-open-timer (+ pigeon-open-timer 1)))

  (define (crow-timer-increment)
    (set! crow-timer (+ crow-timer 1)))

  (define (crow-open-timer-increment)
    (set! crow-open-timer (+ crow-open-timer 1)))

  (define (score-stopper-false!)
    (set! score-stopper #f))

  (define (minus-life!)
    (set! lives (- lives 1)))

  (define (animation-blocker-true)
    (set! animation-blocker #t))

  (define (animation-blocker-false)
    (set! animation-blocker #f))

  (define (inside!)
    (set! obstacle-status #t))

  (define (outside!)
    (set! obstacle-status #f))

  (define (dead!) 
    (set! alive-status #f)) 

  (define (alive!)
    (set! alive-status #t))

  (define (change-angle!)
    (set! angle (random-angle)))


  (define (move! angle) ;Find next position using trigonometry
    (let* ((vx (* duck-velocity (cos (degrees->radians angle))))
           (vy (* duck-velocity (sin (degrees->radians angle))))
           (duqx (position 'x))
           (duqy (position 'y)))
      (cond ((eq? tag 'duck) (cond ((and (eq? alive-status #f) (not obstacle-status))  ((position 'y!) (+ duqy (* time 3)))) ;If it's alive keep bouncing, otherwise go straight downwards
                                   (else ((position 'x!) (+ duqx (* vx time duck-velocity)))
                                         ((position 'y!) (+ duqy (* vy time duck-velocity))))))
            ((eq? tag 'chicken) (cond ((and (eq? alive-status #f) (not obstacle-status)) ((position 'y!) (+ duqy (* time 3))))
                                      (else ((position 'x!) (+ duqx (* duck-velocity duck-velocity time))))))
            ((eq? tag 'pigeon) (cond ((and (eq? alive-status #f) (not obstacle-status))  ((position 'y!) (+ duqy (* time 3)))) ;If it's alive keep bouncing, otherwise go straight downwards
                                     (else (cond ((and pigeon-closer (> duqy (- game-y floor-coordinate)) (eq? bird-randomiser 1)) (set! allow-timer #t)
                                                                                                                      (set! current-wall 0)
                                                                                                                      (change-angle!)
                                                                                                                      (set! pigeon-closer #f)
                                                                                                                      (set! pigeon-timer 0)
                                                                                                                      (set! allow-open-timer #t))
                                                 ((and allow-timer (< pigeon-timer pigeon-timer-rest)) 'null)
                                                 (else ((position 'x!) (+ duqx (* vx time pigeon-velocity)))
                                                       ((position 'y!) (+ duqy (* vy time pigeon-velocity))))))))
            ((eq? tag 'crow) (cond ((and (eq? alive-status #f) (not obstacle-status))  ((position 'y!) (+ duqy (* time 3)))) ;If it's alive keep bouncing, otherwise go straight downwards
                                   (else (cond ((and crow-closer obstacle-status (< crow-randomiser 2)) (set! allow-crow-timer #t)
                                                                                                        (change-angle!)
                                                                                                        (set! crow-closer #f)
                                                                                                        (set! crow-timer 0)
                                                                                                        (set! allow-crow-open-timer #t))
                                               ((and allow-crow-timer (< crow-timer crow-timer-rest) 'null))
                                               (else ((position 'x!) (+ duqx (* vx time crow-velocity)))
                                                     ((position 'y!) (+ duqy (* vy time crow-velocity)))))))))))

  (define (reset-pigeon)
    (cond ((> pigeon-open-timer 420) (if (eq? bird-randomiser1 1)
                                         (begin (set! pigeon-closer #t)
                                                (set! pigeon-open-timer (make-negative 200)))))))

  (define (reset-crow)
    (cond ((> crow-open-timer 420) (if (eq? crow-randomiser1 1)
                                       (begin (set! crow-closer #t)
                                              (set! crow-open-timer (make-negative 200)))))))

  (define (pigeon-logic)
    (if allow-timer
        (pigeon-timer-increment))
    (if allow-open-timer
        (pigeon-open-timer-increment)))

  (define (crow-logic)
    (if allow-crow-timer
        (crow-timer-increment))
    (if allow-crow-open-timer
        (crow-open-timer-increment)))

  (define (duck-movement)
    (walls)
    (cond (wall-touching? (change-angle!)
                          (move! angle))
          (else (move! angle))))
 

  (define (random-angle) ;Generate a random angle, based on what wall it is on
    (cond ((eq? current-wall 0) (random 180 360))
          ((eq? current-wall 1) (if (eq? (random 2) 0)
                                    (random 270 360)
                                    (random 0 90)))
          ((eq? current-wall 2) (random 0 180))
          ((eq? current-wall 3) (random 90 270))))

    
  (define (walls) ;Contains all the logic needed for walls, keeps information about if a wall has been hit, and keeps track of the current-wall duck is on
    (let ((duqx (position 'x))
          (duqy (position 'y)))
      (cond ((> duqy game-y) (set! wall-touching? #t) (set! wall-counter (+ wall-counter 1)) (set! current-wall 0) (set! first-wall-touched #t))
            ((< duqx 0) (set! wall-touching? #t) (set! wall-counter (+ wall-counter 1)) (set! current-wall 1) (set! first-wall-touched #t))
            ((< duqy 0) (set! wall-touching? #t) (set! wall-counter (+ wall-counter 1)) (set! current-wall 2) (set! first-wall-touched #t))
            ((> duqx game-x) (set! wall-touching? #t) (set! wall-counter (+ wall-counter 1)) (set! current-wall 3) (set! first-wall-touched #t))
            (else (set! wall-touching? #f)))))
  

  (define (dispatch-duck msg)
    (cond ((eq? msg 'position) position)
          ((eq? msg 'tag) tag)
          ((eq? msg 'wall-touching?) wall-touching?)
          ((eq? msg 'wall-counter) wall-counter)
          ((eq? msg 'random-angle) (random-angle))
          ((eq? msg 'angle) angle)
          ((eq? msg 'change-angle!) (change-angle!))
          ((eq? msg 'walls) (walls))
          ((eq? msg 'move!) move!)
          ((eq? msg 'dead!) (dead!))
          ((eq? msg 'alive!) (alive!))
          ((eq? msg 'alive-status) alive-status)
          ((eq? msg 'obstacle-status) obstacle-status)
          ((eq? msg 'inside!) (inside!))
          ((eq? msg 'outside!) (outside!))
          ((eq? msg 'animation-blocker) animation-blocker)
          ((eq? msg 'animation-blocker-false) (animation-blocker-false))
          ((eq? msg 'animation-blocker-true) (animation-blocker-true))
          ((eq? msg 'minus-life!) (minus-life!))
          ((eq? msg 'lives) lives)
          ((eq? msg 'pigeon-timer) pigeon-timer)
          ((eq? msg 'allow-timer) allow-timer)
          ((eq? msg 'pigeon-on-ground) pigeon-on-ground)
          ((eq? msg 'pigeon-open-timer) pigeon-open-timer)
          ((eq? msg 'reset-pigeon) (reset-pigeon))
          ((eq? msg 'pigeon-logic) (pigeon-logic))
          ((eq? msg 'duck-movement) (duck-movement))
          ((eq? msg 'obstacle-randomiser) obstacle-randomiser)
          ((eq? msg 'score-stopper) score-stopper)
          ((eq? msg 'score-stopper-false!) (score-stopper-false!))
          ((eq? msg 'crow-logic) (crow-logic))
          ((eq? msg 'reset-crow) (reset-crow))
          ((eq? msg 'watered) watered)
          ((eq? msg 'water-yes) (water-yes))
          ((eq? msg 'water-no) (water-no))
          ((eq? msg 'make-slow) (make-slow))
          ((eq? msg 'caught-in-net?) caught-in-net?)
          ((eq? msg 'net-yes) (net-yes))
          ((eq? msg 'net-no) (net-no))
          ((eq? msg 'removed) removed)
          ((eq? msg 'removed!) (removed!))
          (else (error "Duck ADT - Unkown message" msg))))
  
  dispatch-duck)