(ns bouncing-balls.modes.regular.state
  (:import [java.awt Color])
  (:require [bouncing-balls.utils.general :refer [random-value]]
            [bouncing-balls.modes.regular.data :refer :all]
            [bouncing-balls.modes.regular.fns :refer :all]))

(defn generate-random-balls  [ball-count [min-radius max-radius]]
  (vec (map
        (fn [_]
          {:x (random-value (apply max x-range))
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
              (generate-random-balls (Math/round (* 0.975 particle-count)) [2 12])
              (generate-random-balls (Math/round (* 0.025 particle-count)) [12 48])
              ;; (generate-random-balls (Math/round (* 0.1 particle-count)) [36 48])
              )))

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
       (apply-gravity)
       (apply-collisions)
       (mapv apply-boundary-bounce)
       (mapv last)))
