(ns bouncing-balls.modes.regular.data)

(def frames-per-second 120)
(def ticks-per-second (/ 1000 frames-per-second))
(def particle-count 100)
(def particle-size 32)
(def initial-velocity 1) ;; px per tick
(def x-range [0 1200])
(def y-range [0 800])
(def bounce-velocity-loss 0.75)
(def gravity {
              :on true
              :dynamic true
              :x 50
              :y Double/POSITIVE_INFINITY
              :force 0.2
})