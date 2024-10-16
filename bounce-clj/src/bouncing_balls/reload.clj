(ns bouncing-balls.reload
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [bouncing-balls.main :refer [-main]]))

(defn reload-and-run []
  (refresh)
  (-main))

(reload-and-run)
