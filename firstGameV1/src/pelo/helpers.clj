(ns pelo.helpers)

(defn now [] (new java.util.Date))
(defn now-unix [] (System/currentTimeMillis))

(defn factorial [n] (reduce *' (range 1 (inc n))))

(defn extract-nth [n c]
  [(nth c n) (into (vec (take n c)) (vec (drop (inc n) c)))])

(defn safe-divide [a b & [default]]
  (try
    (/ a b)
    (catch ArithmeticException e
      default)))

(defn average-mean [coll]
  (/ (reduce + coll) (count coll)))

(defn move-towards-zero [num mv]
  (if (= (int num) 0)
    0
    (let [abs-num (Math/abs num)
          dir (/ num abs-num)
          abs-mv (Math/abs mv)
          abs-diff (- abs-num abs-mv)] (* dir (max abs-diff 0)))))


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
(defn get-pairwise-combination-indexes [n]
  (map (fn [idx] [idx (vec (range (inc idx) n))]) (range n)))

(defn pairwise-combination [vect func]
  (let [el (nth vect 0)
        rest-vect-slice (drop 1 vect)
        result (into
                (mapv #(func el %) rest-vect-slice)
                (if (> (count vect) 2)
                  (pairwise-combination rest-vect-slice func)
                  []))] result))

(defn indexed-pairwise-combination [vect func & [idx]]
  (let [el (nth vect idx)
        rest-vect-slice (drop (inc idx) vect)
        result (into
                (map-indexed (fn [i itm]
                            ;;    [[idx (+ i idx)] (func el itm)]) 
                               {:idxs [idx (+ idx i 1)] :vals (func el itm)})
                             rest-vect-slice)
                (if (>= (count rest-vect-slice) 2)
                  (indexed-pairwise-combination vect func (inc idx))
                  []))]
    result))

(defn consolidate-pairwise-combination-result [pwc-result n]
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

(defn consolidate-indexed-pairwise-combination-result [pwc-result n]
  (let [res
        (fill-start-with-nil (mapv
                              (fn [idx]
                                (filter (fn [data]
                                          (let [indexes (:idxs data)
                                                contains-idx (.contains indexes idx)]
                                            contains-idx)) pwc-result))
                              (vec (range n)))
                             n)]

    (mapv (fn [v] (fill-start-with-nil v (dec n))) res)))

(defn get-indexed-pairwise-combination-matrix [pwc-result n]
  (let [res
        (mapv
         (fn [idx]
           (mapv (fn [data]
                   (first (filter #(not= idx %) (:idxs data))))
                 (filter (fn [data]
                           (let [indexes (:idxs data)
                                 contains-idx (.contains indexes idx)]
                             (and contains-idx (and (not= (:vals data) nil) (not= (:vals data) false))))) pwc-result)))
         (vec (range n)))]

    res))

(defn get-index-map-for-indexed-pairwise-combination-result [pwc-result n]
  (mapv
   (fn [idx]
     (map :idx (filter
                (fn [data] (true? (:vals data)))
                pwc-result)))
   (vec (range n))))


