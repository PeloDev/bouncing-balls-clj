(ns pelo.helpers)

(defn factorial [n] (reduce *' (range 1 (inc n))))

(defn safe-divide [a b]
  (try
    (/ a b)
    (catch ArithmeticException e
      nil)))


;; n is the number to select from
;; k is the size of the each unique group we make from n
(defn combination [n k]
  (safe-divide (factorial n) (* (factorial k) (factorial (- n k)))))

;; ==================== Trigonometry ====================
(defn degrees-from-y-x [y x] (- 360 (Math/toDegrees (Math/atan2 y x))))


;; ======================= Vectors =======================
(defn transpose [m]
  (apply mapv vector m))

(defn fill-start-with-nil [v n]
  (vec (concat (vec (repeat (- n (count v)) nil)) v)))

(defn fill-end-with-nil [v n]
  (vec (concat v (vec (repeat (- n (count v)) nil)))))

;; ===================== Procedures =====================
(defn get-pairwise-combinator-indexes [n]
  (map (fn [idx] [idx (vec (range (inc idx) n))]) (range n)))

(defn pairwise-combinator [vect func]
  (let [el (nth vect 0)
        rest-vect-slice (drop 1 vect)
        result (into
                (mapv #(func el %) rest-vect-slice)
                (if (> (count vect) 2)
                  (pairwise-combinator rest-vect-slice func)
                  []))] result))

(defn consolidate-pairwise-combinator-result [pwc-result n]
  (let [res (fill-start-with-nil (mapv
                                  (fn [idx]
                                    (let [drop-from-prev-idx (+ (*  -0.5 idx idx) (* (- n 0.5) idx))
                                          take-in-current-idx (- n 1 idx)]
                                      (vec
                                       (take take-in-current-idx
                                             (drop drop-from-prev-idx
                                                   pwc-result)))))
                                  (vec (range n)))
                                 n)]

    (mapv (fn [v] (fill-start-with-nil v (dec n))) res)))


