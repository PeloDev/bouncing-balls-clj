(ns pelo.firstGameV1
  (:require [pelo.core :as core])
  ;; (:require [pelo.playground :as playground])
  (:gen-class))

(defn -main
  "Run it!"
  [& args]
  (core/start-game))
