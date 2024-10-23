(ns bouncing-balls.modes.supernova.state
  (:import [java.awt Color])
  (:require [bouncing-balls.utils.general :refer [random-value]]
            [bouncing-balls.modes.supernova.data :refer :all]
            [bouncing-balls.modes.supernova.fns :refer :all]))

(def epicenter (atom [(random-value max-x) (random-value max-y)])) ;; TODO: update this each "cycle"

(def states (atom
             (vec (map
                   (fn [_]
                     {:x (random-value max-x)
                      :y (random-value max-y)
                      :angle (random-value 360)
                      :x-velocity nil
                      :y-velocity nil
                      :colour Color/WHITE
                      :gravitaional-center nil ;; TODO
                      })
                   (range particle-count)))))

(defn update-state [states]
  (->> states
       ;; TODO...
       (mapv last)))