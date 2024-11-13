(ns platform-runner.state
  (:require [platform-runner.config :refer [config]]))

(def player (atom {:x (+ 20 (apply max (:x-range (:viewport config))))
                         :y (+ 20 (apply min (:y-range (:viewport config))))}))