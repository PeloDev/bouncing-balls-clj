(ns bouncing-balls.main
  (:require [bouncing-balls.core :as core])
  ;; (:require [pelo.playground :as playground])
  (:gen-class))

(defn -main
  "Run it!"
  [& args]
  (core/start-game))
