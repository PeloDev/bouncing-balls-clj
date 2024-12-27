(ns bouncing-balls.modes.regular.data)

(def frames-per-second 60)
(def ticks-per-second (/ 1000 frames-per-second))
(def particle-count 50)
(def particle-size 32)
(def initial-velocity 3) ;; px per tick
(def x-range [0 1200])
(def y-range [0 800])
(def bounce-velocity-loss 0.25)
(def absorption-probability 100)
(def gravity {
              :on true
              :dynamic true
              :x 50
              :y Double/POSITIVE_INFINITY
              :force 0.9 ;; this is a beautiful value for shifting center of gravity, but too fast to demo
            ;;   :force 0.1
})