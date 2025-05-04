;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Constants                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;; IMPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;
(#%require (only racket random error))
(load "Position ADT.rkt")
(load "Hitbox ADT.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;; GAME DIMENSIONS ;;;;;;;;;;;;;;;;;;;;;;;;;
; Dimensions of the game area
(define game-x 750)
(define game-x-half (/ game-x 2))
(define game-y 750)
(define game-y-half (/ game-y 2))
(define screen-x 158)
(define screen-y 257)
(define floor-coordinate 114)
(define gun-1-coord 15)
(define gun-2-coord 96)
(define gun-3-coord 171)
(define gun-4-coord 246)


;;;;;;;;;;;;;;;;;;;;;;;;; GRAPHICS & DISPLAY CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;
; Dimensions for tiles and placement
(define half-bomb-tile 47.5)
(define half-duck-tile 60)
(define gun-y-placement -10)

;;;;;;;;;;;;;;;;;;;;;;;;; SCORE AND LIFE SETTINGS ;;;;;;;;;;;;;;;;;;;;;;;;;
; Initial settings for player's score and lives
(define initial-lives 5)
(define lives-x 25)
(define initial-score 0)
(define score-x 113)
(define font-size 16)
(define numbers-y-placement 20)

;;;;;;;;;;;;;;;;;;;;;;;;; HITBOX AND INTERACTION RANGES ;;;;;;;;;;;;;;;;;;;;;;;;;
; Ranges for interaction between elements
(define hitbox-range 40)
(define watergun-range 100)
(define netgun-range 180)

;;;;;;;;;;;;;;;;;;;;;;;;; GAMEPLAY TIMING AND VELOCITY CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;
; Time and game speeds
(define time 1.5)

;;;;;;;;;;;;;;;;;;;;;;;;; OBSTACLE DEFINITIONS ;;;;;;;;;;;;;;;;;;;;;;;;;
; Definitions for various obstacles in the game
(define tower1 (hitbox 477 647 276 687)) ; Example TOWER
(define fatcop1 (hitbox 129 252 583 689))
(define airbal1 (hitbox 93 196 136 242))

(define tower2 (hitbox 77 210 587 710)) ; Another TOWER example
(define fatcop2 (hitbox 578 688 362 163))
(define airbal2 (hitbox 84 158 133 201))

(define tower3 (hitbox 0 122 530 699)) ; Yet another TOWER example
(define fatcop3 (hitbox 521 607 573 679))
(define airbal3 (hitbox 0 82 230 330))

;;;;;;;;;;;;;;;;;;;;;;;;; POWERUP SETTINGS ;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants related to powerup behavior
(define powerup-threshold 420)
(define powerup-range 70)

;;;;;;;;;;;;;;;;;;;;;;;;; GAME STATE LISTS ;;;;;;;;;;;;;;;;;;;;;;;;;
; Lists for managing game elements
(define created-ducks '())
(define created-tiles '())
(define created-bombs '())
(define created-bomb-tiles '())
(define created-droplet-tiles '())
(define created-droplets '())
(define created-powerups '())
(define powerup-tiles '())

;;;;;;;;;;;;;;;;;;;;;;;;; TIMERS AND GAMEPLAY LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;
; Timers for animations and gameplay mechanics
(define duck-animation-timer 0)
(define click-timer 0)
(define click-timer-constraint 8)
(define reload-timer 60)
(define bird-click-timer 60)
(define water-click-timer 0)
(define turbo-click-timer 60)
(define turbofixtimer 60)
(define turbo-reload-timer 50)
(define pigeon-timer-rest 100)
(define crow-timer-rest 200)

;;;;;;;;;;;;;;;;;;;;;;;;; WEAPON SETTINGS ;;;;;;;;;;;;;;;;;;;;;;;;;
; Information on ammo and reloading for various weapons
(define current-gun 1)
(define waterammo 5)
(define turboammo 5)
(define turbo-ammo-amount 4)
(define netammo 2)
(define reload-shots 40)
(define reload-shots-amount 40)
(define turbo-shots 240)
(define turbo-shots-amount 240)
(define reload #f)
(define turbotimer #t)
(define net-ammo-amount 2)
(define water-ammo-amount 5)
(define turbo-fix-amount 5)

;;;;;;;;;;;;;;;;;;;;;;;;; BIRD SCORE AMOUNTS ;;;;;;;;;;;;;;;;;;;;;;;;;
(define chicken-score 45)
(define crow-score 50)
(define regular-score 20)
(define pigeon-score 30)
(define extra-score 95)
(define life-normal-amount 5)


;;;;;;;;;;;;;;;;;;;;;;;;; ROUND AND LEVEL CONFIGURATION ;;;;;;;;;;;;;;;;;;;;;;;;;
; Constants for managing rounds and levels
(define round-x 180)
(define totalrounds 6)
(define level-x 255)
(define total-round-number 6)

;;;;;;;;;;;;;;;;;;;;;;;;; GAME STATE MANAGEMENT ;;;;;;;;;;;;;;;;;;;;;;;;;
; Flags and indicators for the game's state
(define game-loop #f)
(define add-once #t)
(define firstlevel #t)
(define secondlevel #t)
(define thirdlevel #t)
(define one-timer1 #t)
(define one-timer2 #t)
(define screen-state 1)
(define screen-stopper #t)
(define one-ammo-timer1 #t)
(define one-ammo-timer2 #t)
(define bg-stopper1 #t)
(define bg-stopper2 #t)
(define obstacle-state 1)
(define one-time-use-power #t)
