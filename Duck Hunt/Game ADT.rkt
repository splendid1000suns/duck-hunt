;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Game                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;IMPORTS;;;;;;;;;;;;;;;;;;;;;;;;;
(#%require "Graphics.rkt")
(#%require math)
(#%require (only racket random error))
(load "Duck ADT.rkt")
(load "Constants.rkt")
(load "Position ADT.rkt")
(load "Gun ADT.rkt")
(load "Bomb ADT.rkt")
(load "Procedures.rkt")
(load "Lives ADT.rkt")
(load "Score ADT.rkt")
(load "Hitbox ADT.rkt")
(load "Draw ADT.rkt")
(load "Droplet ADT.rkt")
(load "Round ADT.rkt")
(load "Powerup ADT.rkt")

  
;;;;;;;;;;;;;;;;;;;;;;;;;INITIALISE BACKGROUND;;;;;;;;;;;;;;;;;;;;;;;;;
(define window (make-window game-x game-y "Duck Test" 200)) ;Make a game window
(define background-layer ((window 'new-layer!))) ;Make background layer
(define background-tile (make-bitmap-tile "Images/GameBackground.png")) ;Make a tile that contains the background
((background-layer 'add-drawable!) background-tile) ;Add background to layer
(define background-tile2 (make-bitmap-tile "Images/GameBackground2.png")) ;Make a tile that contains the background
(define background-tile3 (make-bitmap-tile "Images/GameBackground3.png")) ;Make a tile that contains the background

(define (next-background2)
  ((background-layer 'empty!))
  ((background-layer 'add-drawable!) background-tile2))

(define (next-background3)
  ((background-layer 'empty!))
  ((background-layer 'add-drawable!) background-tile3))

(define (next-background1)
  ((background-layer 'empty!))
  ((background-layer 'add-drawable!) background-tile))


;;;;;;;;;;;;;;;;;;;;SETUP UI & LAYERS;;;;;;;;;;;;;;;;;;;
(define ui-layer ((window 'new-layer!)))
(define homepage-layer ((window 'new-layer!)))
(define object-layer ((window 'new-layer!))) 
(define ui-tile (make-tile game-x game-y))
(define score-tile (make-tile game-x game-y))
(define round-tile (make-tile game-x game-y))
(define level-tile (make-tile game-x game-y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;SETUP RELOAD;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shot-true-tile (make-bitmap-tile "Images/ReloadTrue.png" "Images/ReloadMask.png"))
(define shot-false-tile (make-bitmap-tile "Images/ReloadFalse.png" "Images/ReloadMask.png"))
(define reload-tile (make-tile-sequence (list shot-true-tile shot-false-tile)))
((reload-tile 'set-x!) (- game-x (shot-true-tile 'get-w) 20))
((reload-tile 'set-y!) gun-y-placement)

(define turbo-gun-tile (make-bitmap-tile "Images/TurboGunna.png" "Images/TurboGunnaMask.png"))
(define turbo-gun-false-tile (make-bitmap-tile "Images/TurboGunnaFalse.png" "Images/TurboGunnaMask.png"))
(define turbo-gun-tiles (make-tile-sequence (list turbo-gun-tile turbo-gun-false-tile)))
((turbo-gun-tiles 'set-x!) (- game-x (* (shot-true-tile 'get-w) 2) 20))
((turbo-gun-tiles 'set-y!) gun-y-placement)

(define water-gun-tile (make-bitmap-tile "Images/WaterGun.png" "Images/WaterGunMask.png"))
(define water-gun-false-tile (make-bitmap-tile "Images/WaterGunFalse.png" "Images/WaterGunmask.png"))
(define water-tiles (make-tile-sequence (list water-gun-tile water-gun-false-tile)))
((water-tiles 'set-x!) (- game-x (* (shot-true-tile 'get-w) 3) 20))
((water-tiles 'set-y!) gun-y-placement)

(define net-tile (make-bitmap-tile "Images/NetGun.png" "Images/NetGunMask.png"))
(define net-tile-false (make-bitmap-tile "Images/NetGunFalse.png" "Images/NetGunMask.png"))
(define net-tiles (make-tile-sequence (list net-tile net-tile-false)))
((net-tiles 'set-x!) (- game-x (* (shot-true-tile 'get-w) 4) 14))
((net-tiles 'set-y!) (+ gun-y-placement 8))

(define selectortile (make-bitmap-tile "Images/Selector.png" "Images/SelectorMask.png"))
((selectortile 'set-x!) (- game-x (shot-true-tile 'get-w) 40))
((selectortile 'set-y!) (+ gun-y-placement 12))

(define (selector-rizz)
  (cond ((eq? current-gun 1) ((selectortile 'set-x!) (- game-x (shot-true-tile 'get-w) gun-1-coord)))
        ((eq? current-gun 2) ((selectortile 'set-x!) (- game-x (shot-true-tile 'get-w) gun-2-coord)))
        ((eq? current-gun 3) ((selectortile 'set-x!) (- game-x (shot-true-tile 'get-w) gun-3-coord)))
        ((eq? current-gun 4) ((selectortile 'set-x!) (- game-x (shot-true-tile 'get-w) gun-4-coord)))))



;;;;;;;;;;;;;;;;;;;;;;;;;SETUP LIFE & SCORE;;;;;;;;;;;;;;;;;;;;;;;;;;
(define life-amount (player-life initial-lives))
(define score-amount (player-score initial-score))
((ui-tile 'draw-text!) (number->string (life-amount 'lives)) font-size lives-x numbers-y-placement "black")
((score-tile 'draw-text!) (number->string (score-amount 'score)) font-size score-x numbers-y-placement "black")
((round-tile 'draw-text!) (number->string 1) font-size round-x numbers-y-placement "black")
((level-tile 'draw-text!) (number->string 1) font-size level-x numbers-y-placement "black")
((ui-layer 'add-drawable!) ui-tile)
((ui-layer 'add-drawable!) score-tile)
((ui-layer 'add-drawable!) round-tile)
((ui-layer 'add-drawable!) reload-tile)
((ui-layer 'add-drawable!) water-tiles)
((ui-layer 'add-drawable!) turbo-gun-tiles)
((ui-layer 'add-drawable!) net-tiles)
((ui-layer 'add-drawable!) selectortile)
((ui-layer 'add-drawable!) level-tile)

;;;;;;;;;;;;;;;;;;;;;;SETUP STARTSCREEN;;;;;;;;;;;;;;;;;;;;;;;
(define screen-tile (make-bitmap-tile "Images/StartScreen.png" "Images/StartScreenMaskk.png"))
((screen-tile 'set-x!) screen-x)
((screen-tile 'set-y!) screen-y)
((homepage-layer 'add-drawable!) screen-tile)

(define next-level-tile (make-bitmap-tile "Images/NextLevelScreen.png" "Images/StartScreenMaskk.png"))
((next-level-tile 'set-x!) screen-x)
((next-level-tile 'set-y!) screen-y)

(define beat-game-tile (make-bitmap-tile "Images/BeatGameScreen.png" "Images/StartScreenMaskk.png"))
((beat-game-tile 'set-x!) screen-x)
((beat-game-tile 'set-y!) screen-y)

(define you-died-tile (make-bitmap-tile "Images/YouDiedScreen.png" "Images/StartScreenMaskk.png"))
((you-died-tile 'set-x!) screen-x)
((you-died-tile 'set-y!) screen-y)



;;;;;;;;;;;;;;;;;;;;;SETUP GUN;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gun-layer ((window 'new-layer!))) ;Make gun layer
(define gun-tile (make-bitmap-tile "Images/GameGun.png" "Images/GameGunMask.png")) ;Make tile that contains masked gun
(define watergunicon-tile (make-bitmap-tile "Images/WaterGunindicator.png" "Images/WatergunIndicatorMask.png"))
(define turbogun-tile (make-bitmap-tile "Images/TurboGun.png" "Images/TurboGunMask.png"))
(define netcrosshair (make-bitmap-tile "Images/NetCrosshair.png" "Images/NetCrosshairMask.png"))
((gun-layer 'add-drawable!) gun-tile)
((gun-layer 'add-drawable!) watergunicon-tile);Add gun tile
((gun-layer 'add-drawable!) turbogun-tile)
((gun-layer 'add-drawable!) netcrosshair)


;;;;;;;;;;;;;;;;;;;LEVEL & ROUND LOGIC;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (spawn-round level number type birdamount bombamount)
  (let* ((round (make-round level number type birdamount bombamount)))
    (set! created-rounds (cons round created-rounds))))

(define created-rounds (list (make-round 1 1 'normal 5 3 1)))

(define (double-birds round)
  (round 'double-birds))

(define (update-round round)
  ((round-tile 'clear!))
  ((round-tile 'draw-text!) (number->string (round 'roundnumber)) font-size round-x numbers-y-placement "black")
  ((ui-layer 'add-drawable!) round-tile))

(define (update-level number)
  ((level-tile 'clear!))
  ((level-tile 'draw-text!) (number->string number) font-size level-x numbers-y-placement "black")
  ((ui-layer 'add-drawable!) level-tile))


(define (round-logic round duck)
  (if (< (life-amount 'lives) 1)
      (begin (set! screen-state 3)
             (set! game-loop #f)))
  (cond ((and (< totalrounds 7) firstlevel)
         (cond ((< 0 (round 'birdamount))
                (cond ((or (round 'round-start) (duck 'removed))
                       (if screen-stopper
                           (set! screen-state 2)
                           (set! screen-stopper #f))
                       (round 'round-start-no!)
                       (round 'birdamount-decrement!)
                       (spawn-bird)
                       (if (> (round 'bombamount) 0)
                           (begin (spawn-bomb)
                                  (round 'bombamount-decrement)))
                       (if (> (round 'powerupamount) 0)
                           (begin (spawn-focus-powerup)
                                  (round 'powerup-decrement))))))
               (else (set! created-rounds (list (car (cdr all-rounds)))) ;Next round
                     (set! all-rounds (cdr all-rounds))
                     (set! firstlevel #f)
                     (update-level 2)
                     (next-background2)
                     (set! obstacle-state 2)
                     (set! totalrounds (- totalrounds 1)))))
        ((and (< totalrounds 6) secondlevel)
         (if one-ammo-timer1
             (begin (set! netammo net-ammo-amount)
                    (set! waterammo water-ammo-amount)
                    (set! one-ammo-timer1 #f)))
         (if one-timer1
             (begin (set! game-loop #f)
                    (set! one-timer1 #f)))
         (cond ((< 0 (round 'birdamount))
                (cond ((or (round 'round-start) (duck 'removed))
                       (round 'round-start-no!)
                       (if (eq? (round 'type) 'hard)
                           (begin (if (> (round 'bombamount) 0)
                                      (begin (spawn-bomb)
                                             (round 'bombamount-decrement)))
                                  (if (> (round 'powerupamount) 0)
                                      (begin (spawn-focus-powerup)
                                             (round 'powerup-decrement)))
                                  (begin (set! time 1.6)
                                         (spawn-bird)
                                         (spawn-bird)
                                         (round 'birdamount-decrement!)
                                         (round 'birdamount-decrement!)))
                           (begin (spawn-bird)
                                  (round 'birdamount-decrement!))))))
               (else (set! created-rounds (list (car (cdr all-rounds)))) ;Next round
                     (set! all-rounds (cdr all-rounds))
                     (update-round round)
                     (set! totalrounds (- totalrounds 1))
                     (if (< totalrounds 4)
                         (begin                                 (next-background3)
                                                                (set! obstacle-state 3)
                                                                (update-level 3)
                                                                (set! secondlevel #f))))))
        ((and (< totalrounds 4) thirdlevel)
         (if one-ammo-timer2
             (begin (set! netammo net-ammo-amount)
                    (set! waterammo water-ammo-amount)
                    (set! one-ammo-timer2 #f)))
         (if one-timer2
             (begin (set! game-loop #f)
                    (set! one-timer2 #f)))
         (cond ((< 0 (round 'birdamount))
                (cond ((or (round 'round-start) (duck 'removed))
                       (round 'round-start-no!)
                       (if (eq? (round 'type) 'hard)
                           (begin (set! time 1.6)
                                  (if (> (round 'bombamount) 0)
                                      (begin (spawn-bomb)
                                             (round 'bombamount-decrement)))
                                  (if (> (round 'powerupamount) 0)
                                      (begin (spawn-focus-powerup)
                                             (round 'powerup-decrement)))
                                  (spawn-bird)
                                  (spawn-bird)
                                  (round 'birdamount-decrement!)
                                  (round 'birdamount-decrement!)))
                       (if (eq? (round 'type) 'extreme)
                           (begin (set! time 1.75)
                                  (if (> (round 'bombamount) 0)
                                      (begin (spawn-bomb)
                                             (spawn-bomb)
                                             (spawn-bomb)
                                             (round 'bombamount-decrement)
                                             (round 'bombamount-decrement)))
                                  (spawn-bird)
                                  (spawn-bird)
                                  (round 'birdamount-decrement!)
                                  (round 'birdamount-decrement!))))))
               (else (if (null? (cdr all-rounds))
                         (begin (set! screen-state 4)
                                (set! game-loop #f))
                         (begin (update-round round)
                                (set! created-rounds (list (car (cdr all-rounds)))) 
                                (set! all-rounds (cdr all-rounds))
                                (if (< totalrounds 2)
                                    (set! thirdlevel #f))
                                (set! totalrounds (- totalrounds 1)))))))
        (else (set! game-loop #f))))


(define (resetter)
  (set! screen-state 1)
  (set! created-rounds (list (make-round 1 1 'normal 5 3 1)))
  (set! all-rounds (list (make-round 2 1 'normal 6 2 1)
                         (make-round 2 2 'hard 6 3 2)
                         (make-round 3 1 'hard 6 3 2)
                         (make-round 3 2 'hard 8 4 2)
                         (make-round 3 3 'extreme 12 8 3)))
  (set! firstlevel #t)
  (set! secondlevel #t)
  (set! thirdlevel #t)
  (set! one-timer1 #t)
  (set! one-timer2 #t)
  (set! add-once #t)
  (set! totalrounds total-round-number)
  (score-amount 'reset-score!)
  ((score-tile 'clear!))
  ((score-tile 'draw-text!) (number->string (score-amount 'score)) font-size score-x numbers-y-placement "black")
  ((ui-layer 'add-drawable!) score-tile)
  (life-amount 'reset-lives!)
  ((ui-tile 'clear!))
  ((ui-tile 'draw-text!) (number->string (life-amount 'lives)) font-size lives-x numbers-y-placement "black")
  ((ui-layer 'add-drawable!) ui-tile)
  (update-level 1)
  (update-round (car created-rounds))
  (set! netammo net-ammo-amount)
  (set! waterammo water-ammo-amount)
  (next-background1)
  (set! game-loop #t))


(define (spawn-bird) ;0 1 2 3
  (let ((randomiser (random 4)))
    (cond ((eq? randomiser 0) (spawn-duck))
          ((eq? randomiser 1) (spawn-chicken))
          ((eq? randomiser 2) (spawn-pigeon))
          ((eq? randomiser 3) (spawn-crow)))))


(define all-rounds (list (make-round 2 1 'normal 6 2 1)
                         (make-round 2 2 'hard 6 3 2)
                         (make-round 3 1 'hard 6 3 2)
                         (make-round 3 2 'hard 8 4 2)
                         (make-round 3 3 'extreme 12 8 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;SCREENLOGIC;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-start-screen)
  (if add-once
      (begin ((homepage-layer 'add-drawable!) screen-tile)
             (set! add-once #f))))

(define (show-next-level-screen)
  (if add-once
      (begin ((homepage-layer 'add-drawable!) next-level-tile)
             (set! add-once #f))))

(define (show-you-died-screen)
  (if add-once
      (begin ((homepage-layer 'add-drawable!) you-died-tile)
             (set! add-once #f))))

(define (show-beat-game-screen)
  (if add-once
      (begin ((homepage-layer 'add-drawable!) beat-game-tile)
             (set! add-once #f))))

(define (delete-start-screen)
  ((homepage-layer 'empty!))
  (set! add-once #t))



;;;;;;;;;;;;;;;;;;;;;;;;;SPAWN OBJECTS;;;;;;;;;;;;;;;;;;;;;;;;;
(define (spawn-duck)
  (let* ((duck (make-duck (make-position (random 0 game-x) game-y) 'duck))
         (droplet (make-droplet (make-position 50 50))))
    (set! created-droplets (cons droplet created-droplets))
    (set! created-ducks (cons duck created-ducks))
    ((drawing) 'display-tile-for-duck)
    ((drawing) 'display-tile-for-droplet)))

(define (spawn-chicken)
  (let* ((chicken (make-duck (make-position 0 (- game-y 115)) 'chicken))
         (droplet (make-droplet (make-position 50 50))))
    (set! created-ducks (cons chicken created-ducks))
    (set! created-droplets (cons droplet created-droplets))
    ((drawing) 'display-tile-for-chicken)
    ((drawing) 'display-tile-for-droplet)))

(define (spawn-pigeon)
  (let* ((pigeon (make-duck (make-position (random 0 game-x) 0) 'pigeon))
         (droplet (make-droplet (make-position 50 50))))
    (set! created-ducks (cons pigeon created-ducks))
    (set! created-droplets (cons droplet created-droplets))
    ((drawing) 'display-tile-for-pigeon)
    ((drawing) 'display-tile-for-droplet)))

(define (spawn-crow)
  (let* ((crow (make-duck (make-position (random 0 game-x) game-y) 'crow))
         (droplet (make-droplet (make-position 50 50))))
    (set! created-ducks (cons crow created-ducks))
    (set! created-droplets (cons droplet created-droplets))
    ((drawing) 'display-tile-for-crow)
    ((drawing) 'display-tile-for-droplet)))

(define (spawn-chaos-powerup)
  (let* ((powerup (make-powerup (make-position (random 0 game-x) (random 0 game-y)) 'chaos)))
    (set! created-powerups (cons powerup created-powerups)))
  ((drawing) 'display-tile-for-chaos))

(define (spawn-focus-powerup)
  (let* ((powerup (make-powerup (make-position (random 0 game-x) (random 0 game-y)) 'focus)))
    (set! created-powerups (cons powerup created-powerups)))
  ((drawing) 'display-tile-for-focus))

(define (spawn-bomb)
  (let ((bomb (make-bomb (make-position (random 0 (round (* (/ game-x 3) 2))) game-y))))
    (bomb 'random-top)
    (bomb 'random-width)
    (bomb 'random-sidestep)
    ((bomb 'set-left-zero!) (left-most-zero (find-zeroes (bomb 'top) (bomb 'sidestep) (make-negative (bomb 'width)))))
    ((bomb 'set-spawn-point!) ((bomb 'position) 'x))
    (set! created-bombs (cons bomb created-bombs))
    (if (eq? (random 2) 1)
        ((drawing) 'display-tile-for-bomb)
        ((drawing) 'display-tile-for-flight))))


(define gun (make-gun (make-position game-x-half game-y-half)))

;;;;;;;;;;;;;;;;;;;;;;;;;MOVEMENT;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bomb-movement bomb)
  (bomb 'move!))

;;;;;;;;;;;;;;;;;;;;;;;;;DRAW;;;;;;;;;;;;;;;;;;;;;;;;;
(define (draw duck tile) 
  ((tile 'set-x!) (- ((duck 'position) 'x) half-duck-tile)) 
  ((tile 'set-y!) (- ((duck 'position) 'y) half-duck-tile)))

(define (drawdroplet droplet duck tile)
  ((tile 'set-x!) (- ((duck 'position) 'x) half-duck-tile)) 
  ((tile 'set-y!) (- ((duck 'position) 'y) half-duck-tile)))

(define (drawpowerup powerup tile)
  ((tile 'set-x!) ((powerup 'position) 'x))
  ((tile 'set-y!) ((powerup 'position) 'y)))

(define (draw-gun)
  (cond ((eq? current-gun 1) ((gun-layer 'empty!))
                             ((gun-layer 'add-drawable!) gun-tile)
                             ((gun-tile 'set-x!) (- ((gun 'position) 'x) (car (middlesprite gun-tile))))
                             ((gun-tile 'set-y!) (- ((gun 'position) 'y) (cdr (middlesprite gun-tile)))))
        ((eq? current-gun 3) ((gun-layer 'empty!))
                             ((gun-layer 'add-drawable!) watergunicon-tile)
                             ((watergunicon-tile 'set-x!) (- ((gun 'position) 'x) (car (middlesprite gun-tile))))
                             ((watergunicon-tile 'set-y!) (- ((gun 'position) 'y) (cdr (middlesprite gun-tile)))))
        ((eq? current-gun 2) ((gun-layer 'empty!))
                             ((gun-layer 'add-drawable!) turbogun-tile)
                             ((turbogun-tile 'set-x!) (- ((gun 'position) 'x) (car (middlesprite gun-tile))))
                             ((turbogun-tile 'set-y!) (- ((gun 'position) 'y) (cdr (middlesprite gun-tile)))))
        ((eq? current-gun 4) ((gun-layer 'empty!))
                             ((gun-layer 'add-drawable!) netcrosshair)
                             ((netcrosshair 'set-x!) (- ((gun 'position) 'x) (car (middlesprite gun-tile))))
                             ((netcrosshair 'set-y!) (- ((gun 'position) 'y) (cdr (middlesprite gun-tile)))))))

(define (draw-droplet)
  ((drawing) 'display-tile-for-droplet))


(define (draw bomb tile)
  ((tile 'set-x!) (- ((bomb 'position) 'x) half-bomb-tile)) 
  ((tile 'set-y!) (- ((bomb 'position) 'y) half-bomb-tile)))

(define (dead-duck-animation duck tile)
  (cond ((and (not (duck 'alive-status)) (not (duck 'obstacle-status)) (duck 'animation-blocker))
         ((tile 'set-next!))
         (duck 'animation-blocker-false))))

(define (dead-bomb-animation bomb tile)
  (cond ((and (not (bomb 'alive-status)) (bomb 'animation-blocker))
         ((tile 'set-next!))
         (bomb 'animation-blocker-false))))

(define (draw-powerupss powerup)
  (let ((powerup-tile (((drawing) 'find-tile) powerup 'powerup)))
    (cond ((powerup 'active) ((powerup-tile 'set-current!) 1))
          (else ((powerup-tile 'set-current!) 0)))))

(define (draw-reload)
  (cond ((eq? current-gun 1) (cond ((< reload-timer reload-shots) ((reload-tile 'set-current!) 1))
                                   (else ((reload-tile 'set-current!) 0))))
        ((eq? current-gun 3) (cond ((< waterammo 0) ((water-tiles 'set-current!) 1))
                                   (else ((water-tiles 'set-current!) 0))))
        ((eq? current-gun 2) (cond ((not reload) ((turbo-gun-tiles 'set-current!) 1))
                                   (else ((turbo-gun-tiles 'set-current!) 0))))
        ((eq? current-gun 4) (cond ((< netammo 0) ((net-tiles 'set-current!) 1))
                                   (else ((net-tiles 'set-current!) 0))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;AMMO LOGIC;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (change-waterammo duck)
  (if (< turbofixtimer turbo-fix-amount)
      (set! turbotimer #f)
      (set! turbotimer #t))
  (cond ((and (gun 'shot?) (eq? current-gun 3) turbotimer) (set! waterammo (- waterammo 1))
                                                           (set! turbo-click-timer 0)
                                                           (set! turbofixtimer 0))))

(define (change-turboammo duck)
  (if (< turbofixtimer turbo-fix-amount)
      (set! turbotimer #f)
      (set! turbotimer #t))
  (cond ((and (gun 'shot?) (eq? current-gun 2) turbotimer) (set! turboammo (- turboammo 1))
                                                           (set! turbo-click-timer 0)
                                                           (set! turbofixtimer 0))))

(define (change-netgunammo duck)
  (if (< turbofixtimer turbo-fix-amount)
      (set! turbotimer #f)
      (set! turbotimer #t))
  (cond ((and (gun 'shot?) (eq? current-gun 4) turbotimer) (set! netammo (- netammo 1))
                                                           (set! turbo-click-timer 0)
                                                           (set! turbofixtimer 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LIVES & SCORE UPDATE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (change-lives bomb)
  (let ((bomb-hit (and (gun 'shot?) (((gun 'position) 'in-range?) (bomb 'position) hitbox-range))))
    (cond (bomb-hit (life-amount 'minus-life)
                    ((ui-tile 'clear!))
                    ((ui-tile 'draw-text!) (number->string (life-amount 'lives)) font-size lives-x numbers-y-placement "black")
                    ((ui-layer 'add-drawable!) ui-tile)
                    (set! click-timer 0)
                    ))
    (cond ((and (bomb 'animation-blocker) bomb-hit) (bomb 'dead!)))))


(define (change-score duck)
  (cond ((and (duck 'score-stopper) (not (duck 'alive-status)))
         (cond ((eq? (duck 'tag) 'chicken) (score-amount 'plus-score-chicken))
               ((eq? (duck 'tag) 'crow) (score-amount 'plus-score-crow))
               ((eq? (duck 'tag) 'pigeon) (score-amount 'plus-score-pigeon))
               (else (score-amount 'plus-score)))
         ((score-tile 'clear!))
         ((score-tile 'draw-text!) (number->string (score-amount 'score)) font-size score-x numbers-y-placement "black")
         ((ui-layer 'add-drawable!) score-tile)
         (set! click-timer 0)
         (duck 'score-stopper-false!))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;KILL & REMOVE GAME ELEMENTS;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dead-checker duck)
  (cond ((eq? (duck 'lives) 0) (duck 'dead!))))


(define (set-dead-duck duck) ;This is solely for the dead animation logic
  (let ((duck-shot-normal (and (gun 'shot?) (((gun 'position) 'in-range?) (duck 'position) hitbox-range)))
        (duck-shot-water (and (gun 'shot?) (((gun 'position) 'in-range?) (duck 'position) watergun-range)))
        (duck-shot-net (and (gun 'shot?) (((gun 'position) 'in-range?) (duck 'position) netgun-range))))
    (cond ((eq? current-gun 1) (cond ((and (duck 'animation-blocker) duck-shot-normal (not (duck 'obstacle-status)))
                                      (duck 'minus-life!)
                                      (duck 'change-angle!)
                                      (set! bird-click-timer 0))))
          ((eq? current-gun 3) (cond ((and (duck 'animation-blocker) duck-shot-water (not (duck 'obstacle-status)) (> waterammo 0))
                                      (((drawing) 'draw-droplet) duck)
                                      (duck 'water-yes)
                                      (set! bird-click-timer 0))))
          ((eq? current-gun 2) (cond ((and (duck 'animation-blocker) duck-shot-normal (not (duck 'obstacle-status)))
                                      (duck 'minus-life!)
                                      (duck 'change-angle!)
                                      (set! bird-click-timer 0))))
          ((eq? current-gun 4) (cond ((and (duck 'animation-blocker) duck-shot-net (not (duck 'obstacle-status)) (> netammo 0))
                                      (duck 'net-yes)
                                      (score-amount 'plus-score-extra)
                                      ((score-tile 'clear!))
                                      ((score-tile 'draw-text!) (number->string (score-amount 'score)) font-size score-x numbers-y-placement "black")
                                      ((ui-layer 'add-drawable!) score-tile)
                                      (set! click-timer 0)
                                      (duck 'score-stopper-false!)
                                      (set! bird-click-timer 0)))))))

                                    


(define (delete-duck duck droplet) ;Just delete the duck
  (let ((lstposition1 (index-of created-ducks duck))
        (lstposition2 (index-of created-droplets droplet)))
    (((drawing) 'remove-tile) duck 'duck)
    (((drawing) 'remove-tile) droplet 'droplet)
    (remove-at-index! created-ducks lstposition1)
    (remove-at-index! created-droplets lstposition2)))

(define (delete-powerup powerup)
  (let ((lstposition (index-of created-powerups powerup)))
    (((drawing) 'remove-tile) powerup 'powerup)
    (remove-at-index! created-powerups lstposition)))

(define (remove-duck duck droplet) ;Delete it WHEN ... This happens
  (cond ((or (> ((duck 'position) 'y) (+ game-y 10)) (> (duck 'wall-counter) 4) (duck 'caught-in-net?))
         (duck 'removed!)
         (delete-duck duck droplet))))

(define (delete-bomb bomb) ;Just delete the bomb
  (let ((lstposition (index-of created-bombs bomb)))
    (((drawing) 'remove-tile) bomb 'bomb)
    (remove-at-index! created-bombs lstposition)))

(define (remove-bomb bomb) ;Delete it WHEN ... this happens
  (cond ((or (< ((bomb 'position) 'x) 0)
             (> ((bomb 'position) 'x) game-x)
             (< ((bomb 'position) 'y) 0)
             (> ((bomb 'position) 'y) game-y)))
        (delete-bomb bomb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;POWERUP LOGIC;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (activate-powerup powerup)
  (let ((powerup-shot (and (gun 'shot?) (((gun 'position) 'in-range?) (powerup 'position) powerup-range))))
    (cond ((and powerup-shot (or (eq? current-gun 1) (eq? current-gun 2))) (powerup 'activate!)))))

(define (powerup-controller powerup)
  (cond ((eq? (powerup 'tag) 'chaos) (cond ((powerup 'active) (map double-birds all-rounds)
                                                              (powerup 'start-timer!))
                                           ((not (powerup 'active)) (eq? (powerup 'tag) 'chaos)) (map reset-rounds all-rounds)))
        ((eq? (powerup 'tag) 'focus) (cond ((powerup 'active) (if one-time-use-power
                                                                  (begin (set! waterammo water-ammo-amount)
                                                                         (set! netammo net-ammo-amount)))
                                                              (set! reload-shots (/ reload-shots 2))
                                                              (set! turbo-shots (/ turbo-shots 2))
                                                              (set! one-time-use-power #f)
                                                              (powerup 'start-timer!))
                                           ((not (powerup 'active)) (set! reload-shots reload-shots-amount)
                                                                    (set! turbo-shots turbo-shots-amount))))));If timer gets above, then deactivate ;Need to reset all rounds

(define (time-controller powerup)
  (cond ((> (powerup 'powerup-timer) powerup-threshold) (delete-powerup powerup)
                                                        (powerup 'deactivate!) ;Also remove it from screen
                                                        (set! one-time-use-power #t)
                                                        (powerup 'reset-timer!))
        ((powerup 'timer) (powerup 'timer-increment))))

(define (reset-rounds round)
  (round 'reset-rounds))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EXTRA;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lives-timer)
  (cond ((< click-timer click-timer-constraint)
         'ok)
        (else (map change-lives created-bombs)
              (map change-score created-ducks))))

(define (bird-clicker)
  (cond ((< bird-click-timer click-timer-constraint) 'ok)
        (else (map set-dead-duck created-ducks))))

(define (water-clicker)
  (cond ((< turbo-click-timer click-timer-constraint) 'ok)
        (else (map change-waterammo created-ducks))))

(define (turbo-clicker)
  (cond ((< turbo-click-timer click-timer-constraint) 'ok)
        (else (map change-turboammo created-ducks))))

(define (net-clicker)
  (cond ((< turbo-click-timer click-timer-constraint) 'ok)
        (else (map change-netgunammo created-ducks))))


(define (reloading)
  (cond ((eq? current-gun 1) (if (< reload-timer reload-shots) ;Reload-shots = Reload time between shots
                                 (set! reload #f)
                                 (set! reload #t)))
        ((eq? current-gun 2) (if (< turbo-reload-timer turbo-shots)
                                 (set! reload #f)
                                 (set! reload #t)))))

(define (helperturbo)
  (if (eq? current-gun 2) (if (< turboammo 0) (begin (set! turbo-reload-timer 0)
                                                     (set! turboammo turbo-ammo-amount)))))

;;;;;;;;;;;;;;;;;;;BIRDLOGIC;;;;;;;;;;;;;;;;;;;;;;;
(define (all-birds birds)
  (birds 'pigeon-logic)
  (birds 'duck-movement)
  (birds 'reset-pigeon)
  (birds 'crow-logic)
  (birds 'reset-crow)
  (birds 'make-slow))

;;;;;;;;;;;;;;;;;;OBSTACLES;;;;;;;;;;;;;;;;;
(define (obstacle-checker duck)
  (let ((duqx ((duck 'position) 'x))
        (duqy ((duck 'position) 'y)))
    (cond ((eq? obstacle-state 1) (cond ((or ((tower1 'inside?) duqx duqy)
                                             ((fatcop1 'inside?) duqx duqy)
                                             ((airbal1 'inside?) duqx duqy))(duck 'inside!))
                                        (else (duck 'outside!))))
          ((eq? obstacle-state 2) (cond ((or ((tower2 'inside?) duqx duqy)
                                             ((fatcop2 'inside?) duqx duqy)
                                             ((airbal2 'inside?) duqx duqy))(duck 'inside!))
                                        (else (duck 'outside!))))
          ((eq? obstacle-state 3) (cond ((or ((tower3 'inside?) duqx duqy)
                                             ((fatcop3 'inside?) duqx duqy)
                                             ((airbal3 'inside?) duqx duqy))(duck 'inside!))
                                        (else (duck 'outside!)))))))

(define (obstacle-controller duck)
  (cond ((and (duck 'obstacle-status) (duck 'alive-status)) (((drawing) 'make-invisible) duck))
        (else (((drawing) 'make-visible) duck))))

;;;;;;;;;;;;;;;;;;;;;;;;;GAMELOOP FUNCTION;;;;;;;;;;;;;;;;;;;;;;;;;


(spawn-bird)

((window 'set-update-callback!)
 (lambda (time)
   (cond (game-loop
          (delete-start-screen)
          (set! click-timer (+ click-timer 1))
          (set! bird-click-timer (+ bird-click-timer 1))
          (set! reload-timer (+ reload-timer 1))
          (set! turbo-click-timer (+ turbo-click-timer 1))
          (set! turbo-reload-timer (+ turbo-reload-timer 1))
          (set! turbofixtimer (+ turbofixtimer 1))
          (turbo-clicker)
          (helperturbo)
          (selector-rizz)
          (reloading)
          (lives-timer)
          (bird-clicker) 
          (net-clicker)
          (water-clicker)
          (map all-birds created-ducks)
          (map obstacle-controller created-ducks)
          (map obstacle-checker created-ducks)
          (map bomb-movement created-bombs)
          (map remove-duck created-ducks created-droplets)
          (map remove-bomb created-bombs)
          (map dead-checker created-ducks)
          (map time-controller created-powerups)
          (map drawpowerup created-powerups powerup-tiles)
          (map activate-powerup created-powerups)
          (map powerup-controller created-powerups)
          (map round-logic created-rounds created-ducks)
          (map draw-powerupss created-powerups)
          (draw-reload))
         (else (cond ((eq? screen-state 1) (show-start-screen))
                     ((eq? screen-state 2) (show-next-level-screen))
                     ((eq? screen-state 3) (show-you-died-screen))
                     ((eq? screen-state 4) (show-beat-game-screen)))))))

((window 'set-draw-callback!)
 (lambda ()
   (map draw created-ducks created-tiles)
   (map draw created-bombs created-bomb-tiles)
   (map dead-duck-animation created-ducks created-tiles)
   (map dead-bomb-animation created-bombs created-bomb-tiles)
   (map drawdroplet created-droplets created-ducks created-droplet-tiles)
   (draw-gun)))

((window 'set-key-callback!) 
 (lambda (type key)
   (cond ((and (eq? type 'pressed) (eq? key #\a)) (set! current-gun 1))
         ((and (eq? type 'pressed) (eq? key #\z)) (set! current-gun 2))
         ((and (eq? type 'pressed) (eq? key #\e)) (set! current-gun 3))
         ((and (eq? type 'pressed) (eq? key #\r)) (set! current-gun 4)))
   (cond ((and (eq? type 'pressed) (eq? key #\o)) (set! game-loop #t))
         ((and (eq? type 'pressed) (eq? key #\p)) (set! game-loop #f)))
   (cond ((and (eq? type 'pressed) (eq? key #\h)) (resetter)))))

