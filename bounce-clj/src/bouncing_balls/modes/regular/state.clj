(ns bouncing-balls.modes.regular.state
  (:import [java.awt Color])
  (:require [bouncing-balls.utils.general :refer [random-value]]
            [bouncing-balls.modes.regular.data :refer :all]
            [bouncing-balls.modes.regular.fns :refer :all]))

(def states (atom
             (vec (map
                   (fn [_]
                     {:x (random-value (apply max x-range))
                      :y (random-value (apply max y-range))
                      :angle (random-value 360)
                      :x-velocity nil
                      :y-velocity nil
                      :colour (Color. (+ 100 (random-value 155)) (+ 100 (random-value 155)) (+ 100 (random-value 155)))
                      :ghost-frames 0
                      :radius (+ 8 (random-value 24)) 
                      })
                   (range particle-count)))))

;; (def states (atom
;;              [{:x 100
;;                :y 100
;;                :angle (random-value 360)
;;                :x-velocity 4
;;                :y-velocity 4
;;                :colour Color/WHITE
;;                :ghost-frames 0}
;;               {:x 200
;;                :y 200
;;                :angle (random-value 360)
;;                :x-velocity -4
;;                :y-velocity -4
;;                :colour Color/WHITE
;;                :ghost-frames 0}]))

(defn update-state [states]
  (->> states
       (mapv apply-move)
       (apply-collisions)
       (mapv apply-boundary-bounce)
       (mapv last)))