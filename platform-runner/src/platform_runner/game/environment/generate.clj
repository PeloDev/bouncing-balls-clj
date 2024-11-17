(ns platform-runner.game.environment.generate
  (:import
   [java.awt Color]))

(def static-rectangle-template {:x 0 :y 0 :width 0 :height 0 :colour Color/WHITE})

(defn create-static-rectangle [x y width height]
  (assoc static-rectangle-template :x x :y y :width width :height height))