#lang racket

;;;;*----------------------------------*;;;;
;;;;*       >>> Graphics.rkt  <<<      *;;;;
;;;;* > Programmeerproject 2023-2024 < *;;;;
;;;;*                                  *;;;;
;;;;*         >>  Versie 1  <<         *;;;;
;;;;*                                  *;;;;
;;;;*            Adapted by:           *;;;;
;;;;*           Bjarno Oeyen           *;;;;
;;;;*       Carlos Rojas Castillo      *;;;;
;;;;*                                  *;;;;
;;;;*    Original implementation by:   *;;;;
;;;;*          Brecht De Rooms         *;;;;
;;;;*       Christophe Scholliers      *;;;;
;;;;*                                  *;;;;
;;;;*      Software Languages Lab      *;;;;
;;;;*----------------------------------*;;;;

;; In R5RS projects, include this library using
;; (#%require "Graphics.rkt")

;; In Racket projects, include this library using
;; (require "Graphics.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (only-in compatibility/mlist mlist? list->mlist mlist->list))
;; R5RS's cons-cells are different from Racket's cons-cells... In Racket
;; mutable cons-cells are known as mcons-cells. As this library has been
;; written using Racket, all incoming lists must be converted accordingly.

(require racket/gui/base)
(require racket/string)

(provide make-window
         make-tile
         make-bitmap-tile
         make-tile-sequence
         generate-mask)

;;;; --------------------------------------------------------------------- ;;;;
;;;;  Note: this is NOT a reference on how to code cleanly.                ;;;;
;;;;        this code needs to be cleaned up since we mainly did all the   ;;;;
;;;;        dirty graphics work to achieve a decent drawing efficiency and ;;;;
;;;;        to make sure students don't waste time on this.                ;;;;
;;;; --------------------------------------------------------------------- ;;;;

;;;;############################### WINDOW #####################################

;;;;---------------------------------------------------------------------
;;;; make-window creates a window that accepts tiles and tile-sequences.
;;;; changing the x-value of a tile will update the canvas.
;;;;---------------------------------------------------------------------

(define default-maximum-fps 60)
(define fps-refresh-time 1000)
(define ignore-held-key #t)
(define default-background-colour "black")

(define (make-window w h title (maximum-fps default-maximum-fps))
  (let* ((show-fps #t)
         (fps 0)
         (fps-accum-dt 0)
         (fps-accum-frames 0)
         (delta-time 0)
         (previous-time (current-milliseconds))
         (background-color #f)
         
         ;; Define our dummy keyboard-callback
         (keyboard-callback (lambda (state key) (void)))
         
         ;; Define our dummy update-callback
         (update-callback (lambda (ev) (void)))
         
         ;; Draw callback
         (draw-callback (lambda () (void)))
         
         ;; Define our dummy mouse-move-callback
         (mouse-move-callback (lambda (x y) (void)))
         ;; Define our dummy mouse-click-callback
         (mouse-click-callback (lambda (btn state x y) (void)))
         
         (game-loop (lambda (deltatime events) (void)))
         (game-loop-timer #f)
         (layers '())
         
         (background-string "black")
         
         (closed #f))
    
    ;; Define the paint-callback which is called each frame
    (define (paint-callback canvas dc)
      ;; before we do anything, the game-loop is executed.
      (draw-callback)
      
      ;; Set the background colour (once!)
      (when background-color
        (send dc set-background background-color)
        (set! background-color #f))
      
      ;; Clear everything on the draw context
      (send dc clear)
      
      ;; Draw all layers on each frame
      (for-each (lambda (layer) ((layer 'draw) dc)) layers)
      
      ;; calculate frames per second. 
      (update-fps! delta-time)
      
      ;; Construct the fps string and set the fps in the frame label
      (when show-fps
        (send frame set-label (construct-fps-string title fps))))
    
    ;; Calculate FPS from the time (ms) since last frame
    (define (update-fps! dt)
      (set! fps-accum-dt (+ fps-accum-dt dt))
      (set! fps-accum-frames (+ fps-accum-frames 1))
      (when (> fps-accum-dt fps-refresh-time)
        (set! fps fps-accum-frames)
        (set! fps-accum-frames 0)
        (set! fps-accum-dt (- fps-accum-dt fps-refresh-time))))
    
    ;; Construct FPS string
    (define (construct-fps-string title fps)
      (string-append title
                     " - fps: "
                     (number->string fps)))
    
    (define keyboard-state (make-hasheq))
    (define (handle-keyboard new-state key)
      (define old-state (hash-ref keyboard-state key #f))
      (if (or (eq? key 'wheel-down)
              (eq? key 'wheel-up))
          (keyboard-callback new-state key)
          (when (or (not (eq? old-state new-state))
                    (not ignore-held-key))
            (hash-set! keyboard-state key new-state)
            (keyboard-callback new-state key))))
    
    ;; Make a canvas class that uses our own keyboard callback.
    (define my-canvas%
      (class canvas% ; The base class is canvas%
        ;; Define overriding method to handle keyboard events
        ;; this makes sure our own key-callback is called.
        (define/override (on-char event)
          (define evt (send event get-key-code))
          (if (eq? evt 'release)
              (handle-keyboard 'released (send event get-key-release-code))
              (handle-keyboard 'pressed evt)))
        (define/override (on-event event)
          ;; mouse events
          (let* ([type (send event get-event-type)]
                 [x (send event get-x)]
                 [y (send event get-y)])
            (cond
              [(eq? type 'motion) (mouse-move-callback x y)]
              [(eq? type 'left-down) (mouse-click-callback 'left 'pressed x y)]
              [(eq? type 'left-up) (mouse-click-callback 'left 'released x y)]
              [(eq? type 'middle-down) (mouse-click-callback 'middle 'pressed x y)]
              [(eq? type 'middle-up) (mouse-click-callback 'middle 'released x y)]
              [(eq? type 'right-down) (mouse-click-callback 'right 'pressed x y)]
              [(eq? type 'right-up) (mouse-click-callback 'right 'released x y)])))
        
        ;; Call the superclass init, passing on all init args
        (super-new)))
    
    ;; Make a frame class that can react to closing events
    (define closing-frame%
      (class frame%
        (super-new)
        (define (on-close)
          (set! closed #t))
        (augment on-close)))
    
    ;; Create frame in which we can place a canvas.
    (define frame
      (new closing-frame%
           [label title]
           [width w]
           [height h]))

    ;; Create the canvas with the custom paint-callback 
    ;; This paint-callback is called each time the canvas is refreshed.
    ;; How fast the canvas is refreshed is handled later.
    (define canvas (new my-canvas% 
                        [parent frame]
                        [paint-callback paint-callback] ))
    
    ;; #############################################################    
    ;; ###### public methods for the window ADT ####################
    ;; #############################################################
    ;;Create and add layers to the window
    (define (new-layer!)
      (define layer (make-layer w h canvas))
      (set! layers (append layers (list layer)))
      layer)
    
    ;; Set the backgroudn color of the window
    (define (set-background! str)
      (set! background-string str)
      (set! background-color (make-object color% str)))
    
    ;; #############################################################    
    ;; ###### Setting up a self-sustaining game-loop ###############
    ;; #############################################################    
    ;; Here we handle how fast the canvas is refreshed and thereby how 
    ;; fast paint-callback will be called. 
    (define (launch-game-loop)
      (let* ((min-wait-per-frame 1) ; apparently this has to be at least 1 to avoid locking up.
             (ms-per-frame (quotient 1000 maximum-fps))) ; calculate the MINIMUM delta-time in ms between two frames.
        
        ;; calculate the min delta-time given the min-wait-per-frame
        (define (calculate-interval)
          (truncate (max  
                     (- ms-per-frame delta-time) 
                     min-wait-per-frame)))
        
        ;; The heart of the self-sustaning loop.
        (define (game-loop)  
          ;; get the new delta-time
          (set! delta-time (- (current-milliseconds) previous-time)) 
          ;; We wait for min-delta-time, which is typically the min-wait-per-frame
          (when (>= delta-time ms-per-frame)
            ;; Perform an update...
            (update-callback delta-time)
            ;; calculate actual delta-time. 
            (set! previous-time (current-milliseconds))
            ;; call the canvas refresh which will trigger a paint-callback
            (send canvas refresh-now))
          (when (not closed)
            ;; When the game-loop is done we fire the game-loop again
            ;; after waiting min-delta-time ms, unless the window is closed.
            (send game-loop-timer start (calculate-interval) #t)))
        
        ;; a timer drives the game-loop which calls the game-loop after waiting
        ;; 'interval'. A timer normally calls every 'interval' ms but with
        ;; just-once? #t we prevent that since it will be the game-loop itself
        ;; that will keep itself alive.
        (set! game-loop-timer 
              (new timer% [notify-callback game-loop] 
                   [interval (calculate-interval) ]
                   [just-once? #t]))))

    (define (adjust-size) ;; Some operating systems do not properly initialise the size of the window. This procedure computes a correction, and applies it
      (define-values (size-w size-h) (send frame get-size))
      (define-values (client-size-w client-size-h) (send frame get-client-size))
      ;; (display "user-size: ") (display (list w h)) (newline)
      ;; (display "window-size: ") (display (list size-w size-h)) (newline)
      ;; (display "client-size: ") (display (list client-size-w client-size-h)) (newline)
      (define correction-w (- w client-size-w))
      (define correction-h (- h client-size-h))
      ;; (display "correction: ") (display (list correction-w correction-h)) (newline)
      (send frame resize (+ w correction-w) (+ h correction-h)))
    
    ;; dispatch
    (define (dispatch-window msg)
      (cond ((eq? msg 'new-layer!) new-layer!)
            ((eq? msg 'set-background!) set-background!)
            ((eq? msg 'set-key-callback!) (lambda (eh) (set! keyboard-callback eh)))
            ((eq? msg 'set-update-callback!) (lambda (uc) (set! update-callback uc)))
            ((eq? msg 'set-draw-callback!) (lambda (dc) (set! draw-callback dc)))
            ((eq? msg 'set-mouse-click-callback!) (lambda (mc) (set! mouse-click-callback mc)))
            ((eq? msg 'set-mouse-move-callback!) (lambda (mc) (set! mouse-move-callback mc)))
            ((eq? msg 'set-title!) (lambda (t) (set! title t)))
            ((eq? msg 'get-title) title)
            ((eq? msg 'get-background) background-string)
            (else (raise-arguments-error 'window
                                         "wrong message sent"
                                         "message"
                                         msg)))) 
    
    ;; set background
    (set-background! default-background-colour)
    
    ;; launch the self-sustaining game-loop.
    (launch-game-loop)

    ;; adjust the size of the window
    (adjust-size)

    ;; Show the window
    (send frame show #t)
    (send canvas focus)
    
    ;; Change the mouse event mode
    (send frame wheel-event-mode 'one)
    
    dispatch-window))


;;;;################################ GET SPRITES FROM DISK #######################################
;;;;---------------------------------------------------------------------
;;;; make-bitmap creates a bitmap given a path to an image file
;;;; String -> get-bitmap
;;;;---------------------------------------------------------------------
(define (get-bitmap file)
  (let ((bitmap (make-object bitmap% 1 1)))
    (unless (file-exists? file)
      (error 'get-bitmap "Cannot load file path: ~a" file))
    (send bitmap load-file file)
    bitmap))

;;;;---------------------------------------------------------------------
;;;; make-bitmap creates a bitmap given a path to an image file
;;;; String -> get-bitmap-section
;;;;---------------------------------------------------------------------
(define (get-bitmap-section tilebitmap x y width height)
  (define target-bitmap (make-object bitmap% width height))
  (define target-dc (new bitmap-dc% [bitmap target-bitmap]))
  (send target-dc draw-bitmap-section tilebitmap 0 0 x y width height)
  target-bitmap)

;;;;---------------------------------------------------------------------
;;;; generate-mask generates a mask and saves it to disk.
;;;; String, String -> void
;;;;---------------------------------------------------------------------
(define (generate-mask bitmappath background-color)
  (when (string? background-color) (set! background-color (send the-color-database find-color background-color)))
  (define bitmap (get-bitmap bitmappath))
  (define dc (new bitmap-dc% [bitmap bitmap]))
  (define white-pixel (make-object color% "white"))
  (define black-pixel (make-object color% "black"))
  (printf "Generating mask for ~a...~n" bitmappath)
  (for ([w (send bitmap get-width)])
    (for ([h (send bitmap get-height)])
      (define pixel (make-object color%))
      (send dc get-pixel w h pixel)
      (if (and (= (send background-color red) (send pixel red))
               (= (send background-color blue) (send pixel blue))
               (= (send background-color green) (send pixel green)))
          (send dc set-pixel w h white-pixel)
          (send dc set-pixel w h black-pixel))))
  (define extension (path-get-extension bitmappath))
  (when (not extension) (raise 'unknown-extension))
  (define extension-str (bytes->string/utf-8 extension))
  (define old-suffix extension-str)
  (define new-suffix "_mask.png")
  (define maskpath (string-replace bitmappath old-suffix new-suffix))
  (printf "Saving mask to ~a...~n" bitmappath)
  (define save-result (send (send dc get-bitmap) save-file maskpath 'png))
  (when (not save-result) (raise 'save-failed))
  (void))


;;;;################################ TILES #######################################
;;;;---------------------------------------------------------------------
;;;; make-bitmap-tile creates a tile from a bitmap with optionally a mask.
;;;; [] mean it is optional.
;;;; String, [String] -> Tile
;;;;---------------------------------------------------------------------
(define (make-bitmap-tile bitmappath [mask #f])
  (define bitmap (get-bitmap bitmappath))
  (make-tile (send bitmap get-width) (send bitmap get-height) bitmap mask))

;;;;---------------------------------------------------------------------
;;;; make-tile creates a tile from a width and height with optionally
;;;; a bitmap and a mask. 
;;;; [] mean it is optional.
;;;; Number, Number, [String, [String]] -> Tile
;;;;---------------------------------------------------------------------     
(define (make-tile w h [bitmap #f] [mask #f])
  (when (string? bitmap) (set! bitmap (get-bitmap bitmap)))
  (when (string? mask) (set! mask (get-bitmap mask)))
  (when (not bitmap) (set! bitmap (make-object bitmap% w h #f #t)))
  (define bufferbitmap (make-object bitmap% w h #f #t))
  (let* ((x 0) 
         (y 0)
         (x-scale 1)
         (y-scale 1)
         (mask-dc (new bitmap-dc% [bitmap mask]))
         (update-callback  (lambda () #t))
         (bitmap-dc (new bitmap-dc%  [bitmap bufferbitmap]))
         (rotation 0))

    (define (trigger-update!)
      (update-callback))
    
    (send bitmap-dc draw-bitmap bitmap 0 0)
    
    ;; ##### Drawing methods to draw on the tile yourself.
    ;; Clear removed your own drawings. 
    ;; void -> void
    (define (clear)
      (set! bufferbitmap (make-object bitmap% w h #f #t))
      (set! bitmap-dc (new bitmap-dc%  [bitmap bufferbitmap]))
      (send bitmap-dc draw-bitmap bitmap 0 0)
      (trigger-update!))
    
    ;; Drawing a rectangle
    ;; Number, Number, Number, Number, (String ∪ Color%) -> void
    (define (draw-rectangle x y w h color)
      (when (string? color) (set! color (send the-color-database find-color color)))
      (send bitmap-dc set-brush color 'solid)
      (send bitmap-dc set-pen color 1 'transparent)
      (send bitmap-dc draw-rectangle x y w h)
      (trigger-update!))
    
    ;; Drawing an Ellipse
    ;; Number, Number, Number, Number, (String ∪ Color%) -> void
    (define (draw-ellipse x y w h color)
      (when (string? color) (set! color (send the-color-database find-color color)))
      (send bitmap-dc set-brush color 'solid)
      (send bitmap-dc set-pen color 1 'transparent)
      (send bitmap-dc draw-ellipse x y w h)
      (trigger-update!))
    
    ;; Drawing a Line
    ;; Number, Number, Number, Number, Number, (String ∪ Color%) -> void
    (define (draw-line x y w h width color)
      (when (string? color) (set! color (send the-color-database find-color color)))
      (send bitmap-dc set-pen color width 'solid)
      (send bitmap-dc draw-line x y w h)
      (trigger-update!))
    
    ;; Drawing Text
    ;; String, Number, Number, Number, (String ∪ Color%) -> void
    (define (draw-text text fontsize x y color)
      (when (string? color) (set! color (send the-color-database find-color color)))
      (send bitmap-dc set-font (make-object font% fontsize 'default))
      (send bitmap-dc set-text-foreground color)
      (send bitmap-dc draw-text text x y)
      (trigger-update!))
    
    ;; Rotation of 90 degrees clockwise.
    ;; void -> void
    (define (rotate-clockwise!)
      (rotate! (modulo (+ rotation 90) 360)))
    
    ;; Rotation of 90 degrees counterclockwise.
    ;; void -> void
    (define (rotate-counterclockwise!)
      (rotate! (modulo (- rotation 90) 360)))
    
    ;; Internal Rotation Function with a hack to solve
    ;; the rather bizar way of rotating in the graphical DrRacket library.
    ;; void -> void
    (define (rotate! r)
      (set! rotation r)
      (trigger-update!))
    
    ;; Set the X position on the screen
    ;; number -> void
    (define (set-x! new-x)
      (unless (= x new-x)
        (set! x new-x)
        (trigger-update!)))
    
    ;; Set the Y position on the screen
    ;; number -> void
    (define (set-y! new-y)
      (unless (= y new-y)
        (set! y new-y)
        (trigger-update!)))
    
    (define transparent-color (make-object color% 0 0 0 0))
    
    ;; Drawing procedure called by the layer 
    ;; on which the tile is drawn. This should not be called in a student project!
    ;; dc% -> void
    (define (draw dc)
      (define offset-x (+ x (/ w 2)))
      (define offset-y (+ y (/ h 2)))
      (send dc translate offset-x offset-y)
      (define rotation-r (/ (* rotation pi) 180))
      (send dc rotate rotation-r)
      (send dc set-scale x-scale y-scale)
      (if mask
          (begin (send mask-dc draw-bitmap mask 0 0)
                 (send dc draw-bitmap bufferbitmap (- (/ w 2)) (- (/ h 2)) 'solid transparent-color mask))
          (send dc draw-bitmap bufferbitmap (- (/ w 2)) (- (/ h 2))))
      (send dc set-scale 1 1)
      (send dc rotate (- rotation-r))
      (send dc translate (- offset-x) (- offset-y)))
    
    ;; A procedure to set a callback. This callback
    ;; will notify the parent (layers) that the tile
    ;; has changed and allows us to automatically
    ;; redraw the tiles.
    ;; (void -> void) -> void
    (define (set-on-update! new_callback)
      (set! update-callback new_callback))
    
    ;; Get the scale. If x-scale and y-scale are different, returns the average of the two.
    ;; void -> number
    (define (get-scale)
      (if (= x-scale y-scale)
          x-scale
          (/ (+ x-scale y-scale) 2)))
    
    ;; number -> void
    (define (set-x-scale! s)
      (set! x-scale s))
    
    ;; number -> void
    (define (set-y-scale! s)
      (set! y-scale s))
    
    ;; Sets both scales at once.
    ;; number -> void
    (define (set-scale! s)
      (set-x-scale! s)
      (set-y-scale! s))
        
    ;; Dispatch
    (define (dispatch-tile msg . args)
      (cond 
        ;; Not to be called manually
        ((eq? msg 'draw) draw)
        ((eq? msg 'set-on-update!) set-on-update!)
        
        ;; Getters and setters
        ((eq? msg 'set-x!) set-x!)
        ((eq? msg 'set-y!) set-y!)
        ((eq? msg 'get-x) x)
        ((eq? msg 'get-y) y)
        ((eq? msg 'get-w) w)
        ((eq? msg 'get-h) h)
        
        ;; Rotation
        ((eq? msg 'get-rotation) rotation)
        ((eq? msg 'rotate-clockwise!) rotate-clockwise!)
        ((eq? msg 'rotate-counterclockwise!) rotate-counterclockwise!)
        ((eq? msg 'rotate!) rotate!)
        
        ;; Scale
        ((eq? msg 'set-x-scale!) set-x-scale!)
        ((eq? msg 'set-y-scale!) set-y-scale!)
        ((eq? msg 'set-scale!) set-scale!)
        ((eq? msg 'get-x-scale) x-scale)
        ((eq? msg 'get-y-scale) y-scale)
        ((eq? msg 'get-scale) (get-scale))
        
        ;; Clear whatever is on the tile
        ((eq? msg 'clear!) clear)
        
        ;; Drawing
        ((eq? msg 'draw-rectangle!) draw-rectangle) 
        ((eq? msg 'draw-ellipse!) draw-ellipse) 
        ((eq? msg 'draw-line!) draw-line) 
        ((eq? msg 'draw-text!) draw-text) 
        
        ;; Error if other message is sent
        (else (raise-arguments-error 'tile
                                     "wrong message sent"
                                     "message"
                                     msg))))
    dispatch-tile))

;;;;---------------------------------------------------------------------
;;;; tile-sequence is a sequence of tiles, it is created by passing a list
;;;; of tiles to the tile-sequence. A tile-sequence is meant to animate tiles.
;;;; When it is created, the current tile (index) is set on the first tile that
;;;; was added. Calling next will cycle through the tile-sequence and select the 
;;;; next tile. 
;;;; List<Tile> -> Tile-Sequence
;;;;---------------------------------------------------------------------  
(define (make-tile-sequence tiles-in)
  ;; Initialize the current index and its callback.
  (let ((tiles (if (mlist? tiles-in) (mlist->list tiles-in) tiles-in)) ;; converts mutable list (r5rs) to immutable list (Racket).
        (index 0)
        (update-callback (lambda () #t)))
    
    ;; Change its coordiantes on the window
    ;; Integer -> void
    (define (set-x! new-x)
      (for-each (lambda (tile) ((tile 'set-x!) new-x)) tiles)
      (update-callback))

    ;; Integer -> void
    (define (set-y! new-y)
      (for-each (lambda (tile) ((tile 'set-y!) new-y)) tiles)
      (update-callback))
    
    ;; choose which tile in the sequence is currently active
    ;; by providing an index.
    ;; Integer -> void
    (define (set-current! new_index)
      (if (or (>= new_index (length tiles))
              (< new_index 0))
          (error 'error "illegal index given for tile-sequence: ~a" new_index)
          (begin (set! index new_index)
                 (update-callback))))
    
    ;; Set the previous tile as active tile. 
    ;; void -> void
    (define (set-previous!)
      (set! index (remainder  (- index 1) (length tiles)))
      (when (< index 0) (set! index (- (length tiles) 1)))
      (update-callback))
    
    ;; Set the next tile as active tile. 
    ;; void -> void
    (define (set-next!)
      (set! index (remainder  (+ 1 index) (length tiles)))
      (update-callback))
    
    ;; Drawing functions, each of them will forward the 
    ;; drawing instruction to the underlying tiles.
    ;; void -> void
    (define (rotate-clockwise!)
      (for-each (lambda (tile) (tile 'rotate-clockwise) ) tiles)
      (update-callback))
    
    ;; void -> void
    (define (rotate-counterclockwise!)
      (for-each (lambda (tile) (tile 'rotate-counterclockwise) ) tiles)
      (update-callback))
    
    ;; Number, Number, Number, Number, String -> void
    (define (draw-rectangle x y w h color)
      (for-each (lambda (tile) ((tile 'draw-rectangle) x y w h color )) tiles)
      (update-callback))
    
    ;; Number, Number, Number, Number, String -> void
    (define (draw-ellipse x y w h color)
      (for-each (lambda (tile) ((tile 'draw-ellipse) x y w h color )) tiles)
      (update-callback))
    
    ;; String, Number, Number, Number, String -> void
    (define (draw-text text fontsize x y color)
      (for-each (lambda (tile) ((tile 'draw-text) text fontsize x y color )) tiles)
      (update-callback))
    
    ;; Number, Number, Number, Number, Number, String -> void
    (define (draw-line x y w h width color)
      (for-each (lambda (tile) ((tile 'draw-line)x y w h width color  )) tiles)
      (update-callback))
    
    ;; Clears everything that is drawn by the user, 
    ;; if there were bitmaps, the bitmaps are restored.
    ;; void -> void
    (define (clear)
      (for-each (lambda (tile) (tile 'clear)) tiles)
      (update-callback))
    
    
    ;; redraw itself on the provided drawing context
    ;; void -> void
    (define (draw dc)     
      (((current) 'draw) dc))
    
    ;; set update callback which is called every-time a sequence changes
    ;; (void -> void) -> void
    (define (set-on-update! new_callback)
      (set! update-callback new_callback))
    
    ;; Interal function to retrieve current (private).
    ;; void -> Tile
    (define (current)
      (list-ref tiles index))
    
    (define (rotate! d)
      (for-each (lambda (tile) ((tile 'rotate!) d)) tiles)
      (update-callback))
    
    (define (set-x-scale! s)
      (for-each (lambda (tile) ((tile 'set-x-scale!) s)) tiles)
      (update-callback))
    
    (define (set-y-scale! s)
      (for-each (lambda (tile) ((tile 'set-y-scale!) s)) tiles)
      (update-callback))
    
    (define (set-scale! s)
      (for-each (lambda (tile) ((tile 'set-scale!) s)) tiles)
      (update-callback))
    
    ;; Dispatch
    (define (dispatch-tile-sequence msg)    
      (cond 
        ;; Not to be called manually
        ((eq? msg 'draw)  draw)
        ((eq? msg 'set-on-update!) set-on-update!)
        
        ;; Moving and dimension and position getters.
        ((eq? msg 'set-x!) set-x!)
        ((eq? msg 'set-y!) set-y!)
        ((eq? msg 'get-x) (lambda () ((current) 'get-x)))
        ((eq? msg 'get-y) (lambda () ((current) 'get-y)))
        ((eq? msg 'get-w) (lambda () ((current) 'get-w)))
        ((eq? msg 'get-h) (lambda () ((current) 'get-h)))
        
        ;; Animations to switch between tiles
        ((eq? msg 'set-current!) set-current!)
        ((eq? msg 'get-current) index)
        ((eq? msg 'set-next!) set-next!)
        ((eq? msg 'set-previous!) set-previous!)
        
        ;; Rotation manipulations
        ((eq? msg 'rotate-clockwise!) rotate-clockwise!)
        ((eq? msg 'rotate-counterclockwise!) rotate-counterclockwise!)
        
        ;; Clear all manual drawings
        ((eq? msg 'clear!) clear)
        
        ;; Rotation
        ((eq? msg 'get-rotation) (lambda () ((current) 'get-rotation)))
        ((eq? msg 'rotate!) rotate!)
        
        ;; Scale
        ((eq? msg 'set-x-scale!) set-x-scale!)
        ((eq? msg 'set-y-scale!) set-y-scale!)
        ((eq? msg 'set-scale!) set-scale!)
        ((eq? msg 'get-x-scale) (lambda () ((current) 'get-x-scale)))
        ((eq? msg 'get-y-scale) (lambda () ((current) 'get-y-scale)))
        ((eq? msg 'get-scale) (lambda () ((current) 'get-scale)))
        
        ;; Create manual drawings
        ((eq? msg 'draw-rectangle!) draw-rectangle) 
        ((eq? msg 'draw-ellipse!) draw-ellipse) 
        ((eq? msg 'draw-line!) draw-line) 
        ((eq? msg 'draw-text!) draw-text) 
        
        ;; Error
        (else (raise-arguments-error 'tile-sequence
                                     "wrong message sent"
                                     "message"
                                     msg))))
    dispatch-tile-sequence))

;;;;################################ LAYER #######################################
;;;;---------------------------------------------------------------------
;;;; layers in a window, each layer has a temporary bitmap. 
;;;; Integer Integer canvas% -> Layer
;;;;---------------------------------------------------------------------  
(define (make-layer w h canvas)
  
  (let* ((drawables '())                              ;; all drawables on this layer.
         (bitmap (make-object bitmap% w h #f #t ))    ;; buffer-bitmap for fast drawing
         (bitmap-dc (new bitmap-dc% [bitmap bitmap])) ;; dc of bitmap (drawing context)
         (needs-update #t))                           ;; even faster drawing thanks to dirty bit.
    
    ;; redraw on temporary bitmap layer.
    ;; void -> void
    (define (redraw)
      (send bitmap-dc erase)
      
      ;; This will redraw all drawables on the layer
      ;; Therefore it is not wise to put one moving object together with a bunch 
      ;; of non-moving objects on ONE layer.
      (for-each (lambda (tile) ((tile 'draw) bitmap-dc)) drawables))
    
    ;; draw itself on given drawing context.
    ;; dc% -> void
    (define (draw dc)
      (when needs-update 
        (redraw) 
        (set! needs-update #f))
      (send dc draw-bitmap bitmap 0 0))
    
    ;; Adds a drawable to the layer which is a tile a tile-sequence or 
    ;; a drawable created by the student which suports 'draw' and 'set-on-update!'
    ;; (Tile ∪ Tile-Sequence) -> void
    (define (add-drawable drawable)
      ((drawable 'set-on-update!) (lambda () (set! needs-update #t)))
      (set! drawables (cons drawable drawables))
      (set! needs-update #t))
    
    ;; Remove a drawable to the layer which is a tile a tile-sequence or 
    ;; a drawable created by the student which suports 'draw' and 'set-on-update!'
    ;; (Tile ∪ Tile-Sequence) -> void
    (define (remove-drawable drawable)
      ((drawable 'set-on-update!) (lambda () #t))
      (set! drawables (remq drawable drawables))
      (set! needs-update #t))

    ;; Removes all drawables from a single layer.
    ;; void -> void
    (define (empty!)
      (for-each remove-drawable drawables))
    
    ;; # dispatch
    (define (dispatch-layer msg)
      (cond ((eq? msg 'add-drawable!)  add-drawable)
            ((eq? msg 'remove-drawable!) remove-drawable)
            ((eq? msg 'empty!) empty!)
            ((eq? msg 'draw) draw)
            (else (raise-arguments-error 'layer
                                         "wrong message sent"
                                         "message"
                                         msg))))
    dispatch-layer)) 
