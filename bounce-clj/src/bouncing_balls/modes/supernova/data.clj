(ns bouncing-balls.modes.supernova.data)

(def frames-per-second 60)
(def ticks-per-second (/ 1000 frames-per-second))
(def particle-count 48)
(def particle-size 3)
(def initial-velocity 5) ;; px per tick
(def gravitational-force 0.04)
(def max-x (- 800 particle-size))
(def min-x 0)
(def max-y (- 600 particle-size))
(def min-y 0)