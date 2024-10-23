(ns bouncing-balls.modes.regular.state
  (:import [java.awt Color])
  (:require [bouncing-balls.utils.general :refer [random-value]]
            [bouncing-balls.modes.regular.data :refer :all]
            [bouncing-balls.modes.regular.fns :refer :all]))

(def states (atom
             (vec (map
                   (fn [_]
                     {:x (random-value max-x)
                      :y (random-value max-y)
                      :angle (random-value 360)
                      :x-velocity nil
                      :y-velocity nil
                      :colour Color/WHITE
                      :ghost-frames 0})
                   (range particle-count)))))

(defn update-state [states]
  (->> states
       (mapv apply-move)
       (apply-collisions)
       (mapv apply-boundary-bounce)
       (mapv last)))