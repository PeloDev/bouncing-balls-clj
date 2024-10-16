(ns bouncing-balls.helpers)

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

(defn round-to-n-places [num n]
  (/ (Math/round (* num (Math/pow 10 n))) (Math/pow 10 n))
  )

(defn average-mean [coll]
  (/ (reduce + coll) (count coll)))

(defn get-direction [start-num end-num]
  (/ (/ start-num end-num) (Math/abs (/ start-num end-num))))

(defn move-towards-zero [num mv]
  (if (or (= (int num) 0) (> mv (Math/abs num)))
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

(defn get-linear-function [[[x1 y1] [x2 y2]]]
  (let [m (/ (- y2 y1) (- x2 x1))
        c (- y2 (* m x2))]
    (fn [x] (+ (* x m) c))))

(defn get-y-given-x-on-line [line given-x]
  (let [[[x1 y1] [x2 y2]] line
        dy (- y2 y1)
        dx (- x2 x1)
        dx-given (- given-x x1)
        diff-ratio (safe-divide dy dx)]
    (if (nil? diff-ratio)
      nil
      (+ y1 (* diff-ratio dx-given)))))

(defn get-x-given-y-on-line [line given-y]
  (let [[[x1 y1] [x2 y2]] line
        dy (- y2 y1)
        dx (- x2 x1)
        dy-given (- given-y y1)
        diff-ratio (safe-divide dx dy)]
    (if (nil? diff-ratio)
      nil
      (+ x1 (* diff-ratio dy-given)))))


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

(defn perpendicular-parallel-velocity-decomposition [v angle-radians]
  (let [perpendicular-component (* v (Math/cos angle-radians))
        parallel-component (* v (Math/sin angle-radians))]
    [perpendicular-component, parallel-component]))

(defn get-radians-angle-of-corner [corner-point point-a point-b]
  (let [[xp yp] corner-point
        [xa ya] point-a
        [xb yb] point-b
        dxap (- xa xp)
        dxbp (- xb xp)
        dyap (- ya yp)
        dybp (- yb yp)
        uv (+
            (* dxap dxbp)
            (* dyap dybp))
        mag-u (Math/sqrt (+ (* dxap dxap) (* dyap dyap)))
        mag-v (Math/sqrt (+ (* dxbp dxbp) (* dybp dybp)))
        cos-delta (/ uv (* mag-u mag-v))
        radians-angle (Math/acos cos-delta)]
    radians-angle))

(defn get-radian-angle-between-points [[x1 y1] [x2 y2]]
  (Math/atan2 (- y2 y1) (- x2 x1)))

;; (defn apply-collisions [state-transition]
;;   (let [;; -----
;;         [prev-states next-states] (transpose state-transition)
;;         transition-coords (mapv get-x-y-from-state-transition state-transition)

;;         ;; indexed-intersections (indexed-pairwise-combination transition-coords find-simple-circle-collision 0)
;;         ;; collision-index-matrix (get-indexed-pairwise-combination-matrix indexed-intersections (count state-transition))
;;         new-states (mapv
;;                     (fn [idx]
;;                       (let [[current-state-transition-row rest-state-transitions] (extract-nth idx state-transition)
;;                             current-proposed-next-state-row (last current-state-transition-row)]
;;                         (if (> (:ghost-frames current-proposed-next-state-row) 0)
;;                           (assoc current-proposed-next-state-row :ghost-frames (dec (:ghost-frames current-proposed-next-state-row)))
;;                           (let [current-state-line (get-x-y-from-state-transition current-state-transition-row)
;;                                 current-state-center-line (mapv #(get-center-point-from-top-left % particle-size) current-state-line)
;;                                 [current-start current-end] current-state-center-line
;;                                 collision-state-transitions (filterv
;;                                                              (fn [state-transition-row]
;;                                                                (let [other-state-line (get-x-y-from-state-transition state-transition-row)
;;                                                                      other-state-center-line (mapv #(get-center-point-from-top-left % particle-size) other-state-line)
;;                                                                      [other-start other-end] other-state-center-line
;;                                                                      start-distance-to-current (distance-between-points current-start other-start)
;;                                                                      end-distance-to-current (distance-between-points current-end other-end)
;;                                                                      start-off-touching (<= start-distance-to-current particle-size)
;;                                                                      end-up-touching (<= end-distance-to-current particle-size)
;;                                                                      are-converging-or-parallel (<= end-distance-to-current start-distance-to-current)
;;                                                                      are-colliding (or
;;                                                                                     end-up-touching
;;                                                                                     (and start-off-touching are-converging-or-parallel))]
;;                                                                  are-colliding))
;;                                                              rest-state-transitions)

;;                                 ;; new-state-row (if (empty? collision-state-transitions)
;;                                 ;;                 current-proposed-next-state-row
;;                                 ;;                 (assoc current-proposed-next-state-row
;;                                 ;;                        :x-velocity (:x-velocity (last (last collision-state-transitions)))
;;                                 ;;                        :y-velocity (:y-velocity (last (last collision-state-transitions)))
;;                                 ;;                        :ghost-frames (+ (:ghost-frames current-proposed-next-state-row) 320)))
;;                                 ]
;;                             (if (empty? collision-state-transitions)
;;                               current-proposed-next-state-row
;;                               (let [;; TODO determine and get the closest collision from the vector collision-state-transitions
;;                                     ;; then we can maybe just apply collision to that closest particle, instead some-colliding-state-transition below
;;                                     some-colliding-state-transition (first collision-state-transitions)
;;                                     ;; other-state-line (get-x-y-from-state-transition some-colliding-state-transition)
;;                                     ;; other-state-center-line (mapv #(get-center-point-from-top-left % particle-size) other-state-line)
;;                                     ;; [other-start other-end] other-state-center-line
;;                                     ;; start-distance-to-current (distance-between-points current-start other-start)
;;                                     ;; end-distance-to-current (distance-between-points current-end other-end)
;;                                     ;; start-off-touching (<= start-distance-to-current particle-size)
;;                                     ;; end-up-touching (<= end-distance-to-current particle-size)
;;                                     ;; start-off-and-end-up-touching (and start-off-touching end-up-touching)
;;                                     other-x-v (:x-velocity (last some-colliding-state-transition))
;;                                     other-y-v (:y-velocity (last some-colliding-state-transition))
;;                                     ;; dx-direction (get-direction (first current-end) (first other-end))
;;                                     ;; dy-direction (get-direction (last current-end) (last other-end))
;;                                     ;; seperation-v-x (if start-off-and-end-up-touching
;;                                     ;;                  (* 0.1 other-x-v dx-direction)
;;                                     ;;                  0)
;;                                     ;; seperation-v-y (if start-off-and-end-up-touching
;;                                     ;;                  (* 0.1 other-y-v dy-direction)
;;                                     ;;                  0)
;;                                     seperation-v-x 0
;;                                     seperation-v-y 0
;;                                     new-state-row (assoc current-proposed-next-state-row
;;                                                          :x-velocity (:x-velocity (+ other-x-v seperation-v-x))
;;                                                          :y-velocity (:y-velocity (+ other-y-v seperation-v-y))
;;                                                          :ghost-frames (+ (:ghost-frames current-proposed-next-state-row) 16))] new-state-row))))))
;;                     (range (count transition-coords)))]
;;     (transpose [prev-states new-states])))