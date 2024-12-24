(ns bouncing-balls.modes.regular.data)

(def frames-per-second 60)
(def ticks-per-second (/ 1000 frames-per-second))
(def particle-count 16)
(def particle-size 32)
(def initial-velocity 5) ;; px per tick
(def gravitational-force 0.1)
(def x-range [0 800])
(def y-range [0 600])
(def bounce-velocity-loss 0.25)