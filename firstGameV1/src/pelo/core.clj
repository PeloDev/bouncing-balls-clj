(ns pelo.core
  (:import [javax.swing JFrame JPanel Timer]
           [java.awt Graphics Graphics2D Color RenderingHints]
           [java.awt.event ActionListener]
           [java.util Random])
  (:require [pelo.helpers :refer :all]))

(def frames-per-second 30)
(def ticks-per-second (/ 1000 frames-per-second))
(def particle-count 3)
(def particle-size 128)
(def initial-velocity 6) ;; px per tick
(def gravitational-force 0.04)
(def max-x (- 800 particle-size))
(def min-x 0)
(def max-y (- 600 particle-size))
(def min-y 0)
(defn random-value [upper-bound] (.nextInt (Random.) upper-bound))
(defn random-pos-neg [x] (if (= 1 (random-value 2)) x (* x -1)))
(def bounce-velocity-loss 0.02)

(def frame (atom nil)) ; Atom to store the frame

(def states (atom
             (vec (map
                   (fn [_]
                     {:x (random-value max-x)
                      :y (random-value max-y)
                      :angle (random-value 360)
                      :x-velocity nil
                      :y-velocity nil
                      :colour Color/WHITE
                      :ghost-frames 0})
                   (range particle-count)))))

(defn draw-canvas [^Graphics g]
  (let [g2d (doto ^Graphics2D g
              (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON))]
    (.setColor g2d Color/BLACK)
    (.fillRect g2d 0 0 800 600)))

(defn draw-particle [^Graphics g {:keys [x y colour]}]
  (let [g2d (doto ^Graphics2D g
              (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON))]
    (.setColor g2d colour)
    (.fillOval g2d x y particle-size particle-size))) ;; x y represent top left corner, size is diameter (or width and height)


(defn bounce [pos distance boundary]
  (let [allowed-distance (- boundary pos)]
    (+ pos (- (* 2 allowed-distance) distance))))

(defn toggle-direction [dir] (if (= dir -) + -))

(defn vec-contains-value [val v]
  (not= nil (some #(= val %) v)))

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


(defn find-intersection [line-one line-two & {:keys [margin ignore-range x-range] :or {margin 0 ignore-range false x-range nil}}]
  ;; (println "margin: " margin)
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
        ;; y-intercept (if (nil? x-intercept) nil ((get-linear-function line-one) x-intercept))
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

(defn find-collision [line-one line-two]
  (let [p-intersection (find-intersection line-one line-two)]
    (if (nil? p-intersection)
      nil
      (let [margin-of-error (/ particle-size 2)
            [line-one-start line-one-end] line-one
            [line-two-start line-two-end] line-two
            distance-line-one (distance-between-points line-one-start line-one-end)
            distance-line-two (distance-between-points line-two-start line-two-end)
            distance-one-p (distance-between-points line-one-start (vec (vals p-intersection)))
            distance-two-p (distance-between-points line-two-start (vec (vals p-intersection)))
            d-one-p-ratio (/ distance-one-p distance-line-one)
            d-two-p-ratio (/ distance-two-p distance-line-two)
            d-two-p-ratio-margin-err (/ (+ distance-two-p margin-of-error) distance-line-two)]
        (if (and (>= d-one-p-ratio d-two-p-ratio) (<= d-one-p-ratio d-two-p-ratio-margin-err))
          p-intersection
          nil)))))

(defn determine-is-collision [line-one line-two]
  (let [p-intersection (find-intersection line-one line-two)]
    (if (nil? p-intersection)
      false
      (let [margin-of-error (/ particle-size 1)
            [line-one-start line-one-end] line-one
            [line-two-start line-two-end] line-two
            distance-line-one (distance-between-points line-one-start line-one-end)
            distance-line-two (distance-between-points line-two-start line-two-end)
            distance-one-p (distance-between-points line-one-start (vec (vals p-intersection)))
            distance-two-p (distance-between-points line-two-start (vec (vals p-intersection)))
            d-one-p-ratio (/ distance-one-p distance-line-one)
            d-two-p-ratio (/ distance-two-p distance-line-two)
            d-two-p-ratio-margin-err (/ (+ distance-two-p margin-of-error) distance-line-two)]
        (if (and (>= d-one-p-ratio d-two-p-ratio) (<= d-one-p-ratio d-two-p-ratio-margin-err))
          true
          false)))))

(defn get-bottom-right-point-from-top-left [[x y] p-size]
  [(+ p-size x) (+ p-size y)])

(defn get-center-point-from-top-left [[x y] p-size]
  [(+ (/ p-size 2) x) (+ (/ p-size 2) y)])

(defn min-max-vector [num1 num2]
  [(min num1 num2) (max num1 num2)])

(defn get-middle-of-two-numbers [num1 num2]
  (let [[min-num max-num] (min-max-vector num1 num2)]
    (+ min-num (/ (- max-num min-num) 2))))

(defn find-point-of-contact [line-one line-two p-size]
  (let [[[tlsx1 tlsy1] [tlex1 tley1]] line-one
        [[tlsx2 tlsy2] [tlex2 tley2]] line-two
        start-centre-one (get-center-point-from-top-left [tlsx1 tlsy1] p-size)
        start-centre-two (get-center-point-from-top-left [tlsx2 tlsy2] p-size)
        end-centre-one (get-center-point-from-top-left [tlex1 tley1] p-size)
        end-centre-two (get-center-point-from-top-left [tlex2 tley2] p-size)
        distance-between-center-starts (distance-between-points start-centre-one start-centre-two)
        distance-between-center-ends (distance-between-points end-centre-one end-centre-two)
        are-boxes-touching (<= distance-between-center-ends p-size)
        are-boxes-converging (< (Math/abs distance-between-center-ends) (Math/abs distance-between-center-starts))]
    (if (and are-boxes-touching are-boxes-converging)
      {:x (get-middle-of-two-numbers (nth end-centre-one 0) (nth end-centre-two 0))
       :y (get-middle-of-two-numbers (nth end-centre-one 1) (nth end-centre-two 1))}
      nil)))

(defn find-boxlike-intersection [tl-line-one tl-line-two]
  (let [br-line-one (mapv (fn [tl-point] (get-bottom-right-point-from-top-left tl-point particle-size)) tl-line-one)
        br-line-two (mapv (fn [tl-point] (get-bottom-right-point-from-top-left tl-point particle-size)) tl-line-two)
        point-of-contact (find-point-of-contact tl-line-one tl-line-two particle-size)]
    (if (nil? point-of-contact)
      (first
       (filterv #(not= nil %) [(find-collision tl-line-one tl-line-two)
                               (find-collision tl-line-one br-line-two)
                               (find-collision br-line-one tl-line-two)
                               (find-collision br-line-one br-line-two)]))
      point-of-contact)))

(defn get-n-intervals-along-line [n line]
  (let [[[x-start y-start] [x-end y-end]] line
        dx (- x-end x-start)
        dy (- y-end y-start)
        x-interval (/ dx n)
        y-interval (/ dy n)
        x-range (range x-start (+ x-end x-interval) x-interval)
        y-range (range y-start (+ y-end y-interval) y-interval)]
    (mapv vector x-range y-range)))

;; Note to self:
;; The pairwise detection is efficient in that it avoids redundancy of having to
;; calculate the same collisions/interactions twice,
;; but it makes it quite difficult to determine state updates for each particle
;; after the interaction, since the data for 2 of them is shared in one place.
;; Maybe I should just keep it simple - do what may feel inefficient to get the desired outcome,
;; then optimise afterwards.
;; I do need to move on from physics at some point...
(defn find-simple-circle-collision [tl-line-one tl-line-two]
  (let [center-l1 (mapv (fn [tl-point] (get-center-point-from-top-left tl-point particle-size)) tl-line-one)
        center-l2 (mapv (fn [tl-point] (get-center-point-from-top-left tl-point particle-size)) tl-line-two)
        [center-l1-start center-l1-end] center-l1
        [center-l2-start center-l2-end] center-l2
        {d-starts :distance ds-xdir :dirx ds-ydir :diry} (distance-between-points-with-direction center-l1-start center-l2-start)
        {d-ends :distance de-xdir :dirx de-ydir :diry} (distance-between-points-with-direction center-l1-end center-l2-end)
        are-touching-start (<= d-starts particle-size)
        are-touching-end (<= d-ends particle-size)
        are-xs-crossing (not= ds-xdir de-xdir)
        are-ys-crossing (not= ds-ydir de-ydir)
        intersection-point-map (find-intersection center-l1 center-l2)
        are-intersecting (not= nil intersection-point-map)
        are-colliding (or
                       are-touching-end
                       (and (or are-xs-crossing are-ys-crossing) are-touching-start))]
    (if are-colliding
      (if are-intersecting
        (vec (vals intersection-point-map))
        {:x (average-mean [(first center-l1-end) (first center-l2-end)])
         :y (average-mean [(last center-l1-end) (last center-l2-end)])})
      nil)))

(defn find-collision-between-two-paths [tl-line-one tl-line-two]
  (let [center-l1 (mapv (fn [tl-point] (get-center-point-from-top-left tl-point particle-size)) tl-line-one)
        center-l2 (mapv (fn [tl-point] (get-center-point-from-top-left tl-point particle-size)) tl-line-two)
        [center-l1-start center-l1-end] center-l1
        [center-l2-start center-l2-end] center-l2
        distance-between-center-starts (distance-between-points center-l1-start center-l2-start)
        distance-between-center-ends (distance-between-points center-l1-end center-l2-end)
        are-touching (<= distance-between-center-ends particle-size)
        are-boxes-converging (< (Math/abs distance-between-center-ends) (Math/abs distance-between-center-starts))
        full-intersection-point-map (find-d-intersection center-l1 center-l2)
        intersection-point-map (find-intersection center-l1 center-l2)]
    (if are-touching
      (if (nil? full-intersection-point-map)
        {:x (average-mean [(first center-l1-end) (first center-l2-end)])
         :y (average-mean [(last center-l1-end) (last center-l2-end)])}
        full-intersection-point-map)
      (if (or (not are-boxes-converging) (nil? intersection-point-map))
        nil
        (let [intersection-point (vec (vals intersection-point-map))
              point-of-least-distance-1 (if (nil? intersection-point) center-l1-end intersection-point)
              point-of-least-distance-2 (if (nil? intersection-point) center-l2-end intersection-point)
              line-1-intervals (get-n-intervals-along-line particle-size [point-of-least-distance-1 (find-point-of-line-extension [center-l1-start point-of-least-distance-1] (- particle-size))])
              line-2-intervals (get-n-intervals-along-line particle-size [point-of-least-distance-2 (find-point-of-line-extension [center-l2-start point-of-least-distance-2] (- particle-size))])
              interval-distances (mapv
                                  (fn [p1 p2 idx]
                                    {:d (distance-between-points p1 p2) :i idx})
                                  line-1-intervals
                                  line-2-intervals
                                  (range (inc particle-size)))
              collision-point-candidate (reduce (fn [closest-d-map d-map]
                                                  (if (<
                                                       (Math/abs (- particle-size (:d d-map)))
                                                       (Math/abs (- particle-size (:d closest-d-map))))
                                                    d-map
                                                    closest-d-map))
                                                {:i -1 :d Double/POSITIVE_INFINITY}
                                                interval-distances)]
          (if (> (:d collision-point-candidate) (* particle-size 1.1))
            nil
            {:c1 (nth line-1-intervals (:i collision-point-candidate)) :c2 (nth line-2-intervals (:i collision-point-candidate))}))))))

(defn determine-is-collision-between-paths [tl-line-one tl-line-two]
  (let [[tl-l1-start tl-l1-end] tl-line-one
        [tl-l2-start tl-l2-end] tl-line-two
        br-line-one (mapv (fn [tl-point] (get-bottom-right-point-from-top-left tl-point particle-size)) tl-line-one)
        br-line-two (mapv (fn [tl-point] (get-bottom-right-point-from-top-left tl-point particle-size)) tl-line-two)
        c-line-one (mapv (fn [tl-point] (get-center-point-from-top-left tl-point particle-size)) tl-line-one)
        c-line-two (mapv (fn [tl-point] (get-center-point-from-top-left tl-point particle-size)) tl-line-two)
        perp-tl-l1-start [[(nth tl-line-one 0)] []]
        point-of-contact (find-point-of-contact tl-line-one tl-line-two particle-size)]
    (if (nil? point-of-contact)
      (= true
         (some true? [(determine-is-collision tl-line-one tl-line-two)
                      (determine-is-collision tl-line-one br-line-two)
                      (determine-is-collision br-line-one tl-line-two)
                      (determine-is-collision br-line-one br-line-two)]))
      true)))


(comment
  ;; thread macros and "let" ->>
  (let [a 2
        b 3
        c "4"]
    (let [d (+ a b)
          e (- a b)
          f (str c "!")]
      [a b c d e f]))

  (->>
   [a b c d e f]
   (let [d (+ a b)
         e (- a b)
         f (str c "!")])
   (let [a 2
         b 3
         c "4"]))
;; <<-
  (def bmtemp (apply list (mapv first [[5 1] [2 3]])))
  (interpose 3 bmtemp)
  (apply > (interpose 10 bmtemp))
  (apply min  bmtemp)
  (find-intersection [[4 8] [4.1 2]] [[0 6] [8 6]] 0)
  (vec (vals {:x 1 :y 4}))
  (distance-between-points [124.12881727411087 492.753424602454] [161.02517063820397 530.5023619097949])
  (perpendicular-parallel-velocity-decomposition 10 (Math/toRadians 0))
  (Math/toDegrees (get-radian-angle-between-points [0 0] [2 2]))
  (Math/toDegrees (get-radians-angle-of-corner [2 3] [5 7] [6 4]))
  (Math/round 0.00003)
  (round-to-n-places 0.035 2)
  (get-y-given-x-on-line [[5 0] [5 10]] 4)
  (get-y-given-x-on-line [[0 5] [10 5]] 40)
  (get-n-intervals-along-line 10 [[4 8] [12 2]])

  (safe-divide 10 0 Double/POSITIVE_INFINITY)

  (def my-pos-inf Double/POSITIVE_INFINITY)
  (= Double/POSITIVE_INFINITY my-pos-inf)

  (def test-ln [[4 4] [6 6]])
  (find-point-of-line-extension test-ln (- (/ (Math/sqrt 8) 2)))
  (def extended-ln [(find-point-of-line-extension (reverse test-ln)  (Math/sqrt 8)) (find-point-of-line-extension test-ln  (Math/sqrt 8))])
  extended-ln

  (move-towards-zero -10 -20)
  (defn test-list-contains-nil []
    (let [c1 nil
          c2 482.45260794893187
          m1 nil
          m2 0.14739205107713113] (vec-contains-value nil [m1 m2 c1 c2])))
  (test-list-contains-nil)


  (def centerl1 [[395.9946988953032 363.66050303503505] [395.9946988953032 361.4784899866583]])
  (def centerl2 [[347.38209083799813 322.79004153440144] [347.38209083799813 319.54440924353503]])

  (defn test-my-fn []
    (let [my-fn (fn [n] (* n n 2))]
      (my-fn 8)))

  (test-my-fn)

  (find-intersection [[0 0] [10 10]] [[0 10] [10 0]])
  (find-d-intersection [[0 0] [10 10]] [[0 10] [10 0]])


  (transpose [[1 2] [3 4] [5 6]])
  (transpose [[1 2 3 4 5 6]])

  (def line-1 [[269 315] [270 317.04]])
  (def line-2 [[123 328] [124 330.04]])
  (find-intersection line-1 line-2)
  (factorial 5)
  (combination 5 2)
  (mapv + [11 5 2])

  (- 2 (apply list [1 2]))

  (pairwise-combination [2 3 8 9 0] -) ;; So beautiful!
  (indexed-pairwise-combination [2 3 8 9 0] - 0)

  (def my-vec [2 3 8 9 0])
  (def my-pwc (pairwise-combination my-vec -))
  my-pwc
  (def conol-my-pwc (consolidate-pairwise-combination-result my-pwc (count my-vec)))
  conol-my-pwc

  (def consol-my-pwc-matrix (mapv (fn [v] (fill-start-with-nil v (dec (count my-vec)))) conol-my-pwc))
  consol-my-pwc-matrix

  (def my-idxd-pwc (indexed-pairwise-combination my-vec - 0))
  my-idxd-pwc
  (def conol-my-idxd-pwc (consolidate-indexed-pairwise-combination-result my-idxd-pwc (count my-vec)))
  conol-my-idxd-pwc

  (transpose consol-my-pwc-matrix)
  (concat [1 2 3] [4 5 6])

  (def my-pwc-idxs (get-pairwise-combination-indexes (count my-vec)))
  my-pwc-idxs

  (contains? [0 2] 2) ;; https://clojuredocs.org/clojure.core/contains_q
  (.contains [0 2] 2) ;; Java one works as expected

  (count my-vec)
  (map-indexed (fn [idx itm] [idx itm]) my-vec)
  (count my-pwc-idxs)
  (count conol-my-pwc)
  (vec (repeat (count my-vec) []))
  (reduce (fn [acc [idx val]]
            ;; {:idx idx :val val}
            (update acc idx conj val)
            (mapv (fn [idx2]) (range (count val))))
          (vec (repeat (count my-vec) []))
          conol-my-pwc)


  (combination 4 3)

  (find-point-of-contact [0 4] [4 4] 4)

  ;; END
  nil)


(defn get-x-y-from-state [state]
  (mapv state [:x :y]))

(defn get-x-y-from-state-transition [[prev-state next-state]]
  [(get-x-y-from-state prev-state) (get-x-y-from-state next-state)])

(comment

  (defn test-stuff [] (let [test-state      (vec (map
                                                  (fn [_]
                                                    {:x (random-value 30)
                                                     :y (random-value 30)
                                                     :angle (random-value 360)
                                                     :x-velocity (random-pos-neg 8)
                                                     :y-velocity (random-pos-neg 8)
                                                     :colour Color/WHITE})
                                                  (range 5)))
                            test-state-transition (->> test-state
                                                       (mapv apply-move))
                            test-state-x-ys (mapv get-x-y-from-state-transition test-state-transition)
                            ;; test-intersections (indexed-pairwise-combination test-state-x-ys find-boxlike-intersection 0)
                            ;; test-intersections (indexed-pairwise-combination test-state-x-ys determine-is-collision-between-paths 0)
                            test-intersections (indexed-pairwise-combination test-state-x-ys find-collision-between-two-paths 0)
                            consolidated-test-intersections (consolidate-indexed-pairwise-combination-result test-intersections (count test-state-transition))
                            consolidated-test-intersections-matrix (get-indexed-pairwise-combination-matrix test-intersections (count test-state-transition))]
                        (println (str "test-state" test-state))
                        (println (str  "test-state-transition" test-state-transition))
                        (println (str "test-state-x-ys" (count test-state-x-ys) test-state-x-ys))
                        (println (str "test-intersections" (count test-intersections) test-intersections))
                        ;; (println (str "test-intersections-bool" (count test-intersections-bool) test-intersections-bool))
                        (println (str "consolidated-test-intersections" (count consolidated-test-intersections) consolidated-test-intersections))
                        (println (str "consolidated-test-intersections-matrix" (count consolidated-test-intersections-matrix) consolidated-test-intersections-matrix))
                        consolidated-test-intersections-matrix))

  (test-stuff)
  (some true? [false false false])
  (def test-consolidated-data [[nil nil nil nil] [nil {:x 291/2, :y 107.03999999999999} {:x 145, :y 91.53999999999999}] [{:x 145, :y 91.53999999999999} nil] [nil] [nil {:x 145, :y 91.53999999999999} nil]])
  (def test-consolidated-row [nil nil {:x 291/2, :y 107.03999999999999} {:x 145, :y 91.53999999999999} {:x 291/2, :y 107.03999999999999} {:x 145, :y 91.53999999999999} nil])
  (vec (distinct test-consolidated-row))
  (mapv #(not= (some identity %) nil) test-consolidated-data)
  (not= (some identity [nil nil nil nil]) nil)
  (some identity test-consolidated-data)
  ;; 
  (random-value 2)
  (and true true true false true)
  (def da (distance-between-points [4 8] [8 4]))
  (def db (distance-between-points [3 3] [10 10]))
  (def p (find-intersection [[4 8] [8 4]] [[3 3] [10 10]]))
  (def dap (distance-between-points [4 8] (vec (vals p))))
  (def dbp (distance-between-points [3 3] (vec (vals p))))
  dap
  dbp
  (/ (+ dap (/ 1 2)) da)
  (/ dbp db)
  (find-collision [[4 8] [8 4]] [[3 3] [10 10]])
  (find-point-of-contact
   [[386.9021130325903 243.6580339887499] [391.9021130325903 248.6580339887499]]
   [[81.2036300463041 547.6372710200945] [86.2036300463041 552.6372710200945]]
   particle-size)
  (get-pairwise-combination-indexes 5)
  (def my-init)
  (def my-map {:a 3 :b 4 :c "4"})
  (:a my-map)
  (contains? [1 2 3 4] 2)
  (extract-nth 2 [1 2 3 4 5])
  (take 2 [1 2 3 4])
  (into [1 2] [3 4])
  ;; END
  )


(defn apply-move [state]
  (let [{:keys [x y angle x-velocity y-velocity]} state
        new-x-velocity (if (nil? x-velocity) (* initial-velocity (Math/cos (Math/toRadians angle))) x-velocity)
        new-y-velocity (+ (if (nil? y-velocity)
                            (* initial-velocity (Math/sin (Math/toRadians angle)))
                            y-velocity) gravitational-force)
        new-x (+ x new-x-velocity)
        new-y (+ y new-y-velocity)
        next-state (assoc state :x new-x :y new-y :x-velocity new-x-velocity :y-velocity new-y-velocity) ;; TODO: use new-x-velocity
        ]
    [state next-state]))


(defn apply-bounce [[old-state new-state]]
  (let [{old-x :x old-y :y old-angle :angle} old-state
        {new-x :x new-y :y new-x-velocity :x-velocity new-y-velocity :y-velocity new-angle :angle} new-state
        ;; -----
        is-x-upper-bounce (>= new-x max-x)
        is-x-lower-bounce (<= new-x min-x)
        is-y-upper-bounce (>= new-y max-y)
        is-y-lower-bounce (<= new-y min-y)
        is-x-bounce (or is-x-upper-bounce is-x-lower-bounce)
        is-y-bounce (or is-y-upper-bounce is-y-lower-bounce)
        dx (- new-x old-x)
        dy (- new-y old-y)
        bounce-x (if is-x-upper-bounce
                   (min (bounce old-x dx max-x) (- max-x 1))
                   (if is-x-lower-bounce
                     (max (bounce old-x dx min-x) (+ min-x 1))
                     new-x))
        bounce-y (if is-y-upper-bounce
                   (min (bounce old-y dy max-y) (- max-y 1))
                   (if is-y-lower-bounce
                     (max (bounce old-y dy min-y) (+ min-y 1))
                     new-y))
        bounce-angle (if is-x-bounce (- 180 new-angle) (if is-y-bounce (- 360 new-angle) new-angle))
        bounce-x-velocity (if is-x-bounce (move-towards-zero (- 0 new-x-velocity) bounce-velocity-loss) new-x-velocity)
        bounce-y-velocity (if is-y-bounce (move-towards-zero (- 0 new-y-velocity) bounce-velocity-loss) new-y-velocity)
        next-state (assoc new-state :x bounce-x :y bounce-y :x-velocity bounce-x-velocity :y-velocity bounce-y-velocity :angle bounce-angle)]
    [new-state next-state]))

(defn debug-paint-red-intersection [state-transition intersection-data]
  (let [[prev-state next-state] state-transition
        updated-next-state (if (not= (some identity intersection-data) nil)
                             (assoc next-state :colour Color/RED)
                             (assoc next-state :colour Color/WHITE))]
    [prev-state updated-next-state]))

(defn apply-collisions-pairwise-combination [state-transition]
  (let [;; -----
        [prev-states next-states] (transpose state-transition)
        transition-coords (mapv get-x-y-from-state-transition state-transition)
        indexed-intersections (indexed-pairwise-combination transition-coords find-simple-circle-collision 0)
        collision-index-matrix (get-indexed-pairwise-combination-matrix indexed-intersections (count state-transition))
        new-state (mapv
                   (fn [state-row collision-indexes]
                     (if (or (> (:ghost-frames state-row) 0) (empty? collision-indexes))
                       (assoc state-row :ghost-frames (min 0 (dec (:ghost-frames state-row))))
                       (let [number-of-collisions (count collision-indexes)
                             avg-colliding-state (reduce
                                                  (fn [avg-state idx]
                                                    ;; (println "avg-state: " avg-state ", idx: " idx)
                                                    (let [state-row-at-idx (nth next-states idx)
                                                          avg-step (fn [k d]
                                                                     (/  (k d) number-of-collisions))]
                                                      (assoc avg-state
                                                             :x (+ (avg-step :x avg-state)
                                                                   (avg-step :x state-row-at-idx))
                                                             :y (+ (avg-step :y avg-state)
                                                                   (avg-step :y state-row-at-idx))
                                                             :x-velocity (+
                                                                          (avg-step :x-velocity avg-state)
                                                                          (avg-step :x-velocity state-row-at-idx))
                                                             :y-velocity (+
                                                                          (avg-step :y-velocity avg-state)
                                                                          (avg-step :y-velocity state-row-at-idx)))))
                                                  {:x 0 :y 0 :x-velocity 0 :y-velocity 0}
                                                  collision-indexes)]
                         (assoc state-row
                                :x-velocity (move-towards-zero (+ 0 (:x-velocity avg-colliding-state)) bounce-velocity-loss);; TODO: replace 0 with `(:x-velocity state-row)` or revert
                                :y-velocity (move-towards-zero (+ 0 (:y-velocity avg-colliding-state)) bounce-velocity-loss);; TODO: replace 0 with `(:y-velocity state-row)` or revert
                                :ghost-frames (+ (:ghost-frames state-row) 16)))))
                   next-states
                   collision-index-matrix)]
    (transpose [prev-states new-state])))

;; We're getting close to the perfect bounce collision. Here are some observation notes:
;; - it appears some collisions only bounce one ball and not the other,
;;   so it appears there is some dominant factor, maybe the angle or direction of approach or something
;; - balls tend to get stuck together and don't move (not sharing velocities correctly?)
;; - balls don't roll of stationary balls, they just stay fixed in place like the point above
(defn bounce-ball [current-state-transition other-state-transitions]
  (let [c-next (last current-state-transition)]
    (if (> (:ghost-frames c-next) 0)
      (assoc c-next :ghost-frames (dec (:ghost-frames c-next)))
      (let [c-line (get-x-y-from-state-transition current-state-transition)
            c-center-line (mapv #(get-center-point-from-top-left % particle-size) c-line)
            [current-start current-end] c-center-line
            collision-point-data (mapv
                                  (fn [other-state-transition-row]
                                    (let [other-state-line (get-x-y-from-state-transition other-state-transition-row)
                                          other-state-center-line (mapv #(get-center-point-from-top-left % particle-size) other-state-line)
                                          [other-start other-end] other-state-center-line
                                          start-distance-to-current (distance-between-points current-start other-start)
                                          end-distance-to-current (distance-between-points current-end other-end)
                                          start-off-touching (<= start-distance-to-current particle-size)
                                          end-up-touching (<= end-distance-to-current particle-size)
                                          are-converging-or-parallel (<= end-distance-to-current start-distance-to-current)
                                          are-colliding (or
                                                         end-up-touching
                                                         (and start-off-touching are-converging-or-parallel))]
                                      (if (not are-colliding)
                                        nil
                                        (let [granularity 20
                                              [[csx csy] [cex cey]] c-center-line
                                              [[osx osy] [oex oey]] other-state-center-line
                                              curr-dx (- cex csx)
                                              curr-dy (- cey csy)
                                              other-dx (- oex osx)
                                              other-dy (- oey osy)
                                              curr-interval-size-x (/ curr-dx granularity)
                                              curr-interval-size-y (/ curr-dy granularity)
                                              other-interval-size-x (/ other-dx granularity)
                                              other-interval-size-y (/ other-dy granularity)
                                              [curr-poc other-poc] (reduce
                                                                    (fn [[c o] granularity-idx]
                                                                      (if (or (nil? c) (nil? o))
                                                                        [[csx csy] [osx osy]]
                                                                        (let [cx-interval-movement (* curr-interval-size-x granularity-idx)
                                                                              cy-interval-movement (* curr-interval-size-y granularity-idx)
                                                                              ox-interval-movement (* other-interval-size-x granularity-idx)
                                                                              oy-interval-movement (* other-interval-size-y granularity-idx)
                                                                              c-coord [(+ csx cx-interval-movement) (+ csy cy-interval-movement)]
                                                                              o-coord [(+ osx ox-interval-movement) (+ osy oy-interval-movement)]
                                                                              d0 (distance-between-points c o)
                                                                              d1 (distance-between-points c-coord o-coord)
                                                                              d0-psize-proximity (Math/abs (- d0 (+ particle-size 0.3)))
                                                                              d1-psize-proximity (Math/abs (- d1 (+ particle-size 0.3)))]
                                                                          (if (< d1-psize-proximity d0-psize-proximity)
                                                                            [c-coord o-coord]
                                                                            [c o]))))
                                                                    [nil nil]
                                                                    (range (inc granularity)))]
                                          [curr-poc other-poc (last other-state-transition-row)]))))
                                  other-state-transitions)
            ;; TODO: don't select first, apply below to all valid collisions
            filtered-collision-point-data (first (filterv #(and (not= nil %)) collision-point-data))
            ]
        (if (nil? filtered-collision-point-data)
          c-next
          (let [[[cx-poc cy-poc] [ox-poc oy-poc] other-next-state] filtered-collision-point-data
                half-p-size (/ particle-size 2)
                collision-angle-radians (get-radian-angle-between-points [cx-poc cy-poc] [ox-poc oy-poc])
                c-next-vel-x (:x-velocity c-next)
                c-next-vel-y (:y-velocity c-next)
                o-next-vel-x (:x-velocity other-next-state)
                o-next-vel-y (:y-velocity other-next-state)
                [c-x-vel-perp c-x-vel-para] (perpendicular-parallel-velocity-decomposition
                                             c-next-vel-x
                                             collision-angle-radians)
                [c-y-vel-perp c-y-vel-para] (perpendicular-parallel-velocity-decomposition
                                             c-next-vel-y
                                             collision-angle-radians)
                [o-x-vel-perp o-x-vel-para] (perpendicular-parallel-velocity-decomposition
                                             o-next-vel-x
                                             collision-angle-radians)
                [o-y-vel-perp o-y-vel-para] (perpendicular-parallel-velocity-decomposition
                                             o-next-vel-y
                                             collision-angle-radians)]
            (assoc c-next
                   :x (- cx-poc half-p-size)
                   :y (- cy-poc half-p-size)
                   :x-velocity (move-towards-zero (- c-x-vel-para o-x-vel-perp) bounce-velocity-loss)
                   :y-velocity (move-towards-zero (- c-y-vel-para o-y-vel-perp) bounce-velocity-loss) 
                   )))))))

(defn apply-collisions [state-transition]
  (let [;; -----
        [prev-states] (transpose state-transition)
        states-count (count state-transition)
        current-rest-state-pairs (mapv #(extract-nth % state-transition) (range states-count))
        new-states (mapv #(apply bounce-ball %) current-rest-state-pairs)
        ]
    (transpose [prev-states new-states])))

(defn update-state [states]
  (->> states
       (mapv apply-move)
       (apply-collisions)
       (mapv apply-bounce)
       (mapv last)))

(defn game-panel []
  (proxy [JPanel ActionListener] []
    (paintComponent [^Graphics g]
      (proxy-super paintComponent g)
      (draw-canvas g)
      (doseq [state @states] (draw-particle g state)))
    (actionPerformed [_]
      (swap! states update-state) ; Update the x position 
      (.repaint this))))

(defn create-frame []
  (when-let [f @frame]
    (.dispose f) ; Close the existing frame if it exists
    (reset! frame nil))  ; Reset the frame atom
  (let [panel (game-panel)
        new-frame (doto (JFrame. (str "Java2D Game Example" initial-velocity))
                    (.setContentPane panel)
                    (.setSize 800 640)
                    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
                    (.setVisible true))
        timer (Timer. ticks-per-second panel)]
        ;; ---

    (.start timer)
    ;; (.start log-timer)
    (reset! frame new-frame))) ; Store the new frame in the atom

(defn start-game []
  (create-frame))