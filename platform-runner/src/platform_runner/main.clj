(ns platform-runner.main
  (:require [platform-runner.core :as core])
  (:gen-class))

(defn -main
  "Run it!"
  [& args]
  (core/start-game))
