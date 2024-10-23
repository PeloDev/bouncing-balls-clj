(ns bouncing-balls.utils.general
  (:import [java.util Random]))

(defn now [] (new java.util.Date))
(defn now-unix [] (System/currentTimeMillis))

(defn vec-contains-value [val v]
  (not= nil (some #(= val %) v)))


(defn random-value [upper-bound] (.nextInt (Random.) upper-bound))
(defn random-pos-neg [x] (if (= 1 (random-value 2)) x (* x -1)))