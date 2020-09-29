(#%require "drawings.scm")

(define L-tile (eval (string->symbol "L-tile") (interaction-environment)))

(define (L-tessellation n)
  (set-tessellation-shift-step!)
  (if (= n 1)
      L-tile
      (let* ((n-half (quotient n 2))
             (tile (L-tessellation n-half)))
        (glue-tiles (glue-tiles tile (transform tile n-half n-half 0))
                    (glue-tiles (transform tile 0 n 1) (transform tile n 0 3))))))

(define (transform tile dx dy dz)
  (case (modulo dz 4)
    ((0) (shift-down (shift-right tile dx) dy))
    ((1) (shift-down (shift-right (quarter-turn-left tile) dx) dy))
    ((2) (shift-down (shift-right (half-turn tile) dx) dy))
    ((3) (shift-down (shift-right (quarter-turn-right tile) dx) dy))))
