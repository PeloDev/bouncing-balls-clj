(ns platform-runner.game.player.state
  (:require [platform-runner.config :refer [config]]))



(def player (atom {:x (+ (apply min (:x-range (:viewport config))) 20)
                   :y (- (apply max (:y-range (:viewport config))) 60)
                   :moving-x 0
                   :moving-y 0
                   :crouching false
                   :poised-to-jump false
                   :falling false
                   :health 100
                   :damage-frames 0 ;; how many frames to stun, couting down
                   :damage-per-frame 0}))

(defn move-player [player]
  (cond
    (not (zero? (:moving-x player))) (assoc player :x (+ (:x player) (:moving-x player)))
    (not (zero? (:moving-y player))) (assoc player :y (+ (:y player) (:moving-y player)))
    :else player))

(defn update-player [player]
  (->> player
       (move-player)
       ;; ...
       ))