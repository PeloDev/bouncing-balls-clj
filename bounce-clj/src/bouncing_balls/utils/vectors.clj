(ns bouncing-balls.utils.vectors)

(defn extract-nth [n c]
  [(nth c n) (into (vec (take n c)) (vec (drop (inc n) c)))])

(defn transpose [m]
  (apply mapv vector m))

(defn fill-start-with-nil [v n]
  (vec (concat (vec (repeat (- n (count v)) nil)) v)))

(defn fill-end-with-nil [v n]
  (vec (concat v (vec (repeat (- n (count v)) nil)))))
