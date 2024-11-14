(ns platform-runner.state
  (:require [platform-runner.config :refer [config]]))



(def player (atom {:x (+ 20 (apply max (:x-range (:viewport config))))
                   :y (+ 20 (apply min (:y-range (:viewport config))))
                   :moving-x 0
                   :moving-y 0
                   :crouching false
                   :poised-to-jump false
                   :falling false
                   :health 100
                   :damage-frames 0 ;; how many frames to stun, couting down
                   :damage-per-frame 0
                   }))