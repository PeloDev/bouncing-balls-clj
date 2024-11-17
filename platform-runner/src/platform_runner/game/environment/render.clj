(ns platform-runner.game.environment.render
  (:require [platform-runner.game.environment.state :refer [environment]])
  (:import
   [java.awt Graphics Graphics2D RenderingHints]))

(defn draw-element [^Graphics g {:keys [x y width height colour]}]
  (let [g2d (doto ^Graphics2D g
              (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON))]
    (.setColor g2d colour)
    (.fillRect g2d x y width height)))

(defn draw-environment [^Graphics g]
  (doseq [element @environment] (draw-element g element)))