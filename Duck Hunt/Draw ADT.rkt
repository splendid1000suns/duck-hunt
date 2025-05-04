;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Draw ADT                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(#%require "Graphics.rkt")
(#%require math)
(#%require (only racket random error))
(load "Constants.rkt")
(load "Procedures.rkt")


(define (drawing)

  (define (display-tile-for-duck) ;Initialises tile to a duck
    (let* ((duck-tile-fly (make-bitmap-tile "Images/FlyDuck.png" "Images/FlyDuckMask.png"))
           (duck-tile-dead (make-bitmap-tile "Images/DeadDuck.png" "Images/DeadDuckMask.png"))
           (duck-tile (make-tile-sequence (list duck-tile-fly duck-tile-dead))))
      (set! created-tiles (cons duck-tile created-tiles))
      ((object-layer 'add-drawable!) duck-tile)))

  (define (display-tile-for-bomb) ;Initialises tile to a bomb
    (let* ((bomb-tile-fly (make-bitmap-tile "Images/Bomb.png" "Images/BombMask.png"))
           (bomb-tile-exploded (make-bitmap-tile "Images/Explosion.png" "Images/ExplosionMask.png"))
           (bomb-tile (make-tile-sequence (list bomb-tile-fly bomb-tile-exploded))))
      (set! created-bomb-tiles (cons bomb-tile created-bomb-tiles))
      ((object-layer 'add-drawable!) bomb-tile)))

  (define (display-tile-for-flight) ;Initialises tile to a flight
    (let* ((bomb-tile-fly (make-bitmap-tile "Images/FalseFlight.png" "Images/FalseFlightMask.png"))
           (bomb-tile-exploded (make-bitmap-tile "Images/FalseFlightExploded.png" "Images/FalseFlightExplodedMask.png"))
           (bomb-tile (make-tile-sequence (list bomb-tile-fly bomb-tile-exploded))))
      (set! created-bomb-tiles (cons bomb-tile created-bomb-tiles))
      ((object-layer 'add-drawable!) bomb-tile)))

  (define (display-tile-for-chicken)
    (let* ((chicken-tile-walk (make-bitmap-tile "Images/chicken.png" "Images/chickenMask.png"))
           (chicken-tile-dead (make-bitmap-tile "Images/Deadchicken.png" "Images/DeadchickenMask.png"))
           (chicken-tile (make-tile-sequence (list chicken-tile-walk chicken-tile-dead))))
      (set! created-tiles (cons chicken-tile created-tiles))
      ((object-layer 'add-drawable!) chicken-tile)))

  (define (display-tile-for-pigeon)
    (let* ((pigeon-tile-walk (make-bitmap-tile "Images/Pigeon.png" "Images/PigeonMask.png"))
           (pigeon-tile-dead (make-bitmap-tile "Images/DeadPigeon.png" "Images/DeadPigeonMask.png"))
           (pigeon-tile (make-tile-sequence (list pigeon-tile-walk pigeon-tile-dead))))
      (set! created-tiles (cons pigeon-tile created-tiles))
      ((object-layer 'add-drawable!) pigeon-tile)))

  (define (display-tile-for-crow)
    (let* ((crow-tile-fly (make-bitmap-tile "Images/Crow.png" "Images/CrowMask.png"))
           (crow-tile-dead (make-bitmap-tile "Images/DeadCrow.png" "Images/DeadCrowMask.png"))
           (crow-tile (make-tile-sequence (list crow-tile-fly crow-tile-dead))))
      (set! created-tiles (cons crow-tile created-tiles))
      ((object-layer 'add-drawable!) crow-tile)))

  (define (display-tile-for-droplet)
    (let* ((droplet-tile (make-bitmap-tile "Images/WaterDroplet.png" "Images/WaterDropletMask.png")))
      (set! created-droplet-tiles (cons droplet-tile created-droplet-tiles))))

  (define (display-tile-for-focus)
    (let* ((focus-tile (make-bitmap-tile "Images/Focus.png" "Images/FocusMask.png"))
           (focus-tile-dead (make-bitmap-tile "Images/FocusActivated.png" "Images/FocusMask.png"))
           (focuss-tile (make-tile-sequence (list focus-tile focus-tile-dead))))
      (set! powerup-tiles (cons focuss-tile powerup-tiles))
      ((object-layer 'add-drawable!) focuss-tile)))

  (define (display-tile-for-chaos)
    (let* ((chaos-tile (make-bitmap-tile "Images/Chaos.png" "Images/ChaosMask.png"))
           (chaos-tile-dead (make-bitmap-tile "Images/ChaosActivated.png" "Images/ChaosMask.png"))
           (chaoss-tile (make-tile-sequence (list chaos-tile chaos-tile-dead))))
      (set! powerup-tiles (cons chaoss-tile powerup-tiles))
      ((object-layer 'add-drawable!) chaos-tile)))

  (define (draw-droplet duck)
    (let* ((lstposition (index-of created-ducks duck))
           (droplet-tile (element-at-index created-droplet-tiles lstposition)))
      ((object-layer 'add-drawable!) droplet-tile)))

  (define (find-tile object tag)
    (cond ((eq? tag 'duck) (let* ((lstposition (index-of created-ducks object))
                                  (tile (element-at-index created-tiles lstposition)))
                             tile))
          ((eq? tag 'bomb) (let* ((lstposition (index-of created-bombs object))
                                  (tile (element-at-index created-bomb-tiles lstposition)))
                             tile))
          ((eq? tag 'droplet) (let* ((lstposition (index-of created-droplets object))
                                     (tile (element-at-index created-droplet-tiles lstposition)))
                                tile))
          ((eq? tag 'powerup) (let* ((lstposition (index-of created-powerups object))
                                     (tile (element-at-index powerup-tiles lstposition)))
                                tile))))

  (define (remove-tile object tag)
    (cond ((eq? tag 'duck) (let ((lstposition (index-of created-ducks object)))
                             ((object-layer 'remove-drawable!) (find-tile object tag))
                             (remove-at-index! created-tiles lstposition)))
          ((eq? tag 'bomb) (let ((lstposition (index-of created-bombs object)))
                             ((object-layer 'remove-drawable!) (find-tile object tag))
                             (remove-at-index! created-bomb-tiles lstposition)))
          ((eq? tag 'droplet) (let ((lstposition (index-of created-droplets object)))
                                ((object-layer 'remove-drawable!) (find-tile object tag))
                                (remove-at-index! created-droplet-tiles lstposition)))
          ((eq? tag 'powerup) (let ((lstposition (index-of created-powerups object)))
                                ((object-layer 'remove-drawable!) (find-tile object tag))
                                (remove-at-index! powerup-tiles lstposition)))))
      

  (define (make-invisible duck)
    (((find-tile duck 'duck) 'set-scale!) 0.01))

  (define (make-visible duck)
    (((find-tile duck 'duck) 'set-scale!) 1))


  (define (dispatch-draw msg)
    (cond ((eq? msg 'find-tile) find-tile)
          ((eq? msg 'display-tile-for-duck) (display-tile-for-duck))
          ((eq? msg 'display-tile-for-bomb) (display-tile-for-bomb))
          ((eq? msg 'display-tile-for-chicken) (display-tile-for-chicken))
          ((eq? msg 'display-tile-for-pigeon) (display-tile-for-pigeon))
          ((eq? msg 'display-tile-for-crow) (display-tile-for-crow))
          ((eq? msg 'display-tile-for-droplet) (display-tile-for-droplet))
          ((eq? msg 'draw-droplet) draw-droplet)
          ((eq? msg 'remove-tile) remove-tile)
          ((eq? msg 'make-invisible) make-invisible)
          ((eq? msg 'make-visible) make-visible)
          ((eq? msg 'display-tile-for-flight) (display-tile-for-flight))
          ((eq? msg 'display-tile-for-chaos) (display-tile-for-chaos))
          ((eq? msg 'display-tile-for-focus) (display-tile-for-focus))
          (else (error "Draw ADT - Unkown message" msg))))
  
  dispatch-draw)