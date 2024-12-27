(ns bouncing-balls.modes.regular.state
  (:import [java.awt Color])
  (:require [bouncing-balls.utils.general :refer [random-value]]
            [bouncing-balls.modes.regular.data :refer :all]
            [bouncing-balls.modes.regular.fns :refer :all]))

(defn generate-random-balls  [ball-count [min-radius max-radius]]
  (vec (map
        (fn [_]
          {:id (random-uuid)
           :x (random-value (apply max x-range))
           :y (random-value (apply max y-range))
           :angle (random-value 360)
           :x-velocity nil
           :y-velocity nil
           :colour (Color. (+ 100 (random-value 155)) (+ 100 (random-value 155)) (+ 100 (random-value 155)))
           :ghost-frames 0
           :radius (+ min-radius (random-value (- max-radius min-radius)))})
        (range ball-count))))

(def states (atom
             (concat
              (generate-random-balls (Math/round (* 0.99 particle-count)) [2 12])
              (generate-random-balls (Math/round (* 0.01 particle-count)) [12 48]))))

(defn update-state [states]
  (->> states
       (mapv apply-move)
       (apply-gravity)
       (apply-collisions)
       (mapv apply-boundary-bounce)
       (mapv last)))
