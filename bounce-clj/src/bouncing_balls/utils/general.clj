(ns bouncing-balls.utils.general)

(defn now [] (new java.util.Date))
(defn now-unix [] (System/currentTimeMillis))

(defn vec-contains-value [val v]
  (not= nil (some #(= val %) v)))
