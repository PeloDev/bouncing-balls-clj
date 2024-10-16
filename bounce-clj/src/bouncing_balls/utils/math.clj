(ns bouncing-balls.utils.math)

(defn factorial [n] (reduce *' (range 1 (inc n))))

(defn safe-divide [a b & [default]]
  (try
    (/ a b)
    (catch ArithmeticException e
      default)))

(defn round-to-n-places [num n]
  (/ (Math/round (* num (Math/pow 10 n))) (Math/pow 10 n)))

(defn average-mean [coll]
  (/ (reduce + coll) (count coll)))

;; n is the number to select from
;; k is the size of the each unique group we make from n
(defn combination [n k]
  (safe-divide (factorial n) (* (factorial k) (factorial (- n k)))))

(defn get-radian-angle-between-points [[x1 y1] [x2 y2]]
  (Math/atan2 (- y2 y1) (- x2 x1)))

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

(defn perpendicular-parallel-velocity-decomposition [v angle-radians]
  (let [perpendicular-component (* v (Math/cos angle-radians))
        parallel-component (* v (Math/sin angle-radians))]
    [perpendicular-component, parallel-component]))

(defn get-direction [start-num end-num]
  (/ (/ start-num end-num) (Math/abs (/ start-num end-num))))

(defn move-towards-zero [num mv]
  (if (or (= (int num) 0) (> mv (Math/abs num)))
    0
    (let [abs-num (Math/abs num)
          dir (/ num abs-num)
          abs-mv (Math/abs mv)
          abs-diff (- abs-num abs-mv)] (* dir (max abs-diff 0)))))

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

(defn get-line-vars [[[x1 y1] [x2 y2]]]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        m (safe-divide dy dx Double/POSITIVE_INFINITY)
        c (if (= m Double/POSITIVE_INFINITY) nil (- y2 (* m x2)))]
    {:m m :c c}))

(defn distance-between-points [point-1 point-2]
  (let [[x1 y1] point-1
        [x2 y2] point-2
        diff-x (- x2 x1)
        diff-y (- y2 y1)]
    (Math/sqrt (+ (* diff-x diff-x) (* diff-y diff-y)))))

(defn distance-between-points-with-direction [point-1 point-2]
  (let [[x1 y1] point-1
        [x2 y2] point-2
        diff-x (- x2 x1)
        diff-y (- y2 y1)]
    {:distance (Math/sqrt (+ (* diff-x diff-x) (* diff-y diff-y)))
     :dirx (if (< diff-x 0) -1 1)
     :diry (if (< diff-y 0) -1 1)}))

(defn find-point-of-line-extension [[[x1 y1] [x2 y2]] extension]
  (let [line-length (distance-between-points [x1 y1] [x2 y2])]
    (if (= 0 (int (Math/round line-length)))
      [x2 y2]
      (let [dx (- x2 x1)
            dy (- y2 y1)
            x-unit (/ dx line-length)
            y-unit (/ dy line-length)
            extended-x (+ x2 (* x-unit extension))
            extended-y (+ y2 (* y-unit extension))]
        [extended-x extended-y]))))

(defn find-d-intersection [line-a line-b]
  (let [[[ax1 ay1] [ax2 ay2]] line-a
        [[bx1 by1] [bx2 by2]] line-b
        a1 (- ay2 ay1)
        b1 (- ax1 ax2)
        c1 (+ (* a1 ax1) (* b1 ay1))
        a2 (- by2 by1)
        b2 (- bx1 bx2)
        c2 (+ (* a2 bx1) (* b2 by1))
        determinant (- (* a1 b2) (* a2 b1))]
    (if (= 0 (int determinant))
      nil
      {:x (/ (- (* c1 b2) (* c2 b1)) determinant)
       :y (/ (- (* c2 a1) (* c1 a2)) determinant)})))

(defn find-intersection [line-one line-two & {:keys [margin ignore-range x-range] :or {margin 0 ignore-range false x-range nil}}]
  (let [r (if (nil? margin) 0 margin)
        ign-range (if (nil? ignore-range) false ignore-range)
        {m1 :m c1 :c} (get-line-vars line-one)
        {m2 :m c2 :c} (get-line-vars line-two)
        m1-is-infinity (= m1 Double/POSITIVE_INFINITY)
        m2-is-infinity (= m2 Double/POSITIVE_INFINITY)
        x-intercept (if m1-is-infinity
                      (nth (nth line-one 0) 0)
                      (if m2-is-infinity
                        (nth (nth line-two 0) 0)
                        (safe-divide (- (+ c2 r) c1) (- m1 m2))))
        y-intercept (if m1-is-infinity
                      (if m2-is-infinity
                        (average-mean (into (into (mapv #(last %) line-one)) (into (mapv #(last %) line-two))))
                        ((get-linear-function line-two) x-intercept))
                      ((get-linear-function line-one) x-intercept))
        line-one-xs-list (apply list (mapv first line-one))
        min-max-xs-one (if (not= nil x-range)
                         (sort x-range)
                         (list (apply min line-one-xs-list) (apply max line-one-xs-list)))
        is-within-range (and
                         (not= x-intercept nil)
                         (apply < (interpose x-intercept min-max-xs-one)))]
    (if (or is-within-range ign-range) {:x x-intercept :y y-intercept} nil)))

(defn min-max-vector [num1 num2]
  [(min num1 num2) (max num1 num2)])

(defn get-middle-of-two-numbers [num1 num2]
  (let [[min-num max-num] (min-max-vector num1 num2)]
    (+ min-num (/ (- max-num min-num) 2))))

(defn get-n-intervals-along-line [n line]
  (let [[[x-start y-start] [x-end y-end]] line
        dx (- x-end x-start)
        dy (- y-end y-start)
        x-interval (/ dx n)
        y-interval (/ dy n)
        x-range (range x-start (+ x-end x-interval) x-interval)
        y-range (range y-start (+ y-end y-interval) y-interval)]
    (mapv vector x-range y-range)))