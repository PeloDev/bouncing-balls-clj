(ns pelo.reload
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [pelo.firstGameV1 :refer [-main]]))

(defn reload-and-run []
  (refresh)
  (-main))

(reload-and-run)
