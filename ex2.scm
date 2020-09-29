(#%require "drawings.scm")

(define (transform tile dx dy dz)
  (case (modulo dz 4)
    ((0) (shift-down (shift-right tile dx) dy))
    ((1) (shift-down (shift-right (quarter-turn-left tile) dx) dy))
    ((2) (shift-down (shift-right (half-turn tile) dx) dy))
    ((3) (shift-down (shift-right (quarter-turn-right tile) dx) dy))))

(set-puzzle-shift-step!)

(glue-tiles (glue-tiles larger-tile (transform smaller-tile 2 0 0))
            (glue-tiles (transform larger-tile 2 1 2) (transform smaller-tile 2 5 2)))

(glue-tiles (glue-tiles (transform larger-tile 0 0 2) (transform smaller-tile 2 0 2))
            (glue-tiles (transform larger-tile 2 1 0) (transform smaller-tile 2 5 0)))
