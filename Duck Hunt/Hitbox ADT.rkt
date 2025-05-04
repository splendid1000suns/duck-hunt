(define (hitbox x1 x2 y1 y2)

  (define (inside? x y)
    (cond ((and (> x x1) (< x x2) (> y y1) (< y y2)) #t)
          (else #f)))

  (define (dispatch-hitbox msg)
    (cond ((eq? msg 'inside?) inside?)
          (else (error "Hitbox ADT - Unkown message" msg))))
  dispatch-hitbox)