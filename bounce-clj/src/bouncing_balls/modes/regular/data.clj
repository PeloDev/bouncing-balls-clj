(ns bouncing-balls.modes.regular.data)

(def frames-per-second 60)
(def ticks-per-second (/ 1000 frames-per-second))
(def particle-count 12)
(def particle-size 27)
(def initial-velocity 5) ;; px per tick
(def gravitational-force 0.08)
(def max-x (- 800 particle-size))
(def min-x 0)
(def max-y (- 600 particle-size))
(def min-y 0)
(def bounce-velocity-loss 0.25)