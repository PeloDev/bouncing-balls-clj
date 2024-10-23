(ns bouncing-balls.data.config.regular)

(def frames-per-second 60)
(def ticks-per-second (/ 1000 frames-per-second))
(def particle-count 24)
(def particle-size 8)
(def initial-velocity 5) ;; px per tick
(def gravitational-force 0.04)
(def max-x (- 800 particle-size))
(def min-x 0)
(def max-y (- 600 particle-size))
(def min-y 0)
(def bounce-velocity-loss 0.004)