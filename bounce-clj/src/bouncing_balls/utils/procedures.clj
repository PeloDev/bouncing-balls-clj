(ns bouncing-balls.utils.procedures
  (:require [bouncing-balls.utils.vectors :refer [fill-start-with-nil]]))

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
