(ns platform-runner.game.environment.state
  (:require [platform-runner.config :refer [config]]
            [platform-runner.game.environment.generate :refer [create-static-rectangle]]))

(def ground-level (apply max (:y-range (:viewport config))))
(def x-start (apply min (:y-range (:viewport config))))
(def x-end (apply max (:x-range (:viewport config))))

(def environment (atom [(create-static-rectangle (- x-start 10) (- ground-level 0) (+ x-end 20) 40)]))
