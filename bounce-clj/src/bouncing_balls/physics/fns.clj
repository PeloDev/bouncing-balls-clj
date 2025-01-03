(ns bouncing-balls.physics.fns
  (:require [bouncing-balls.utils.math :refer [find-intersection
                                               distance-between-points
                                               get-middle-of-two-numbers
                                               distance-between-points-with-direction
                                               average-mean
                                               get-n-intervals-along-line
                                               find-d-intersection
                                               find-point-of-line-extension]]))

(defn get-bottom-right-point-from-top-left [[x y] p-size]
  [(+ p-size x) (+ p-size y)])

(defn get-center-point-from-top-left [[x y] p-size]
  [(+ (/ p-size 2) x) (+ (/ p-size 2) y)])

(defn bounce [pos distance boundary]
  (let [allowed-distance (- boundary pos)]
    (+ pos (- (* 2 allowed-distance) distance))))

(defn find-collision [line-one line-two particle-size]
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

(defn determine-is-collision [line-one line-two particle-size]
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

(defn determine-is-collision-between-paths [tl-line-one tl-line-two p-size]
  (let [[tl-l1-start tl-l1-end] tl-line-one
        [tl-l2-start tl-l2-end] tl-line-two
        br-line-one (mapv (fn [tl-point] (get-bottom-right-point-from-top-left tl-point p-size)) tl-line-one)
        br-line-two (mapv (fn [tl-point] (get-bottom-right-point-from-top-left tl-point p-size)) tl-line-two)
        c-line-one (mapv (fn [tl-point] (get-center-point-from-top-left tl-point p-size)) tl-line-one)
        c-line-two (mapv (fn [tl-point] (get-center-point-from-top-left tl-point p-size)) tl-line-two)
        perp-tl-l1-start [[(nth tl-line-one 0)] []]
        point-of-contact (find-point-of-contact tl-line-one tl-line-two p-size)]
    (if (nil? point-of-contact)
      (= true
         (some true? [(determine-is-collision tl-line-one tl-line-two p-size)
                      (determine-is-collision tl-line-one br-line-two p-size)
                      (determine-is-collision br-line-one tl-line-two p-size)
                      (determine-is-collision br-line-one br-line-two p-size)]))
      true)))

(defn find-boxlike-intersection [tl-line-one tl-line-two particle-size]
  (let [br-line-one (mapv (fn [tl-point] (get-bottom-right-point-from-top-left tl-point particle-size)) tl-line-one)
        br-line-two (mapv (fn [tl-point] (get-bottom-right-point-from-top-left tl-point particle-size)) tl-line-two)
        point-of-contact (find-point-of-contact tl-line-one tl-line-two particle-size)]
    (if (nil? point-of-contact)
      (first
       (filterv #(not= nil %) [(find-collision tl-line-one tl-line-two particle-size)
                               (find-collision tl-line-one br-line-two particle-size)
                               (find-collision br-line-one tl-line-two particle-size)
                               (find-collision br-line-one br-line-two particle-size)]))
      point-of-contact)))

;; Note to self:
;; The pairwise detection is efficient in that it avoids redundancy of having to
;; calculate the same collisions/interactions twice,
;; but it makes it quite difficult to determine state updates for each particle
;; after the interaction, since the data for 2 of them is shared in one place.
;; Maybe I should just keep it simple - do what may feel inefficient to get the desired outcome,
;; then optimise afterwards.
;; I do need to move on from physics at some point...
(defn find-simple-circle-collision [tl-line-one tl-line-two p-size]
  (let [center-l1 (mapv (fn [tl-point] (get-center-point-from-top-left tl-point p-size)) tl-line-one)
        center-l2 (mapv (fn [tl-point] (get-center-point-from-top-left tl-point p-size)) tl-line-two)
        [center-l1-start center-l1-end] center-l1
        [center-l2-start center-l2-end] center-l2
        {d-starts :distance ds-xdir :dirx ds-ydir :diry} (distance-between-points-with-direction center-l1-start center-l2-start)
        {d-ends :distance de-xdir :dirx de-ydir :diry} (distance-between-points-with-direction center-l1-end center-l2-end)
        are-touching-start (<= d-starts p-size)
        are-touching-end (<= d-ends p-size)
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

(defn find-collision-between-two-paths [tl-line-one tl-line-two p-size]
  (let [center-l1 (mapv (fn [tl-point] (get-center-point-from-top-left tl-point p-size)) tl-line-one)
        center-l2 (mapv (fn [tl-point] (get-center-point-from-top-left tl-point p-size)) tl-line-two)
        [center-l1-start center-l1-end] center-l1
        [center-l2-start center-l2-end] center-l2
        distance-between-center-starts (distance-between-points center-l1-start center-l2-start)
        distance-between-center-ends (distance-between-points center-l1-end center-l2-end)
        are-touching (<= distance-between-center-ends p-size)
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
              line-1-intervals (get-n-intervals-along-line p-size [point-of-least-distance-1 (find-point-of-line-extension [center-l1-start point-of-least-distance-1] (- p-size))])
              line-2-intervals (get-n-intervals-along-line p-size [point-of-least-distance-2 (find-point-of-line-extension [center-l2-start point-of-least-distance-2] (- p-size))])
              interval-distances (mapv
                                  (fn [p1 p2 idx]
                                    {:d (distance-between-points p1 p2) :i idx})
                                  line-1-intervals
                                  line-2-intervals
                                  (range (inc p-size)))
              collision-point-candidate (reduce (fn [closest-d-map d-map]
                                                  (if (<
                                                       (Math/abs (- p-size (:d d-map)))
                                                       (Math/abs (- p-size (:d closest-d-map))))
                                                    d-map
                                                    closest-d-map))
                                                {:i -1 :d Double/POSITIVE_INFINITY}
                                                interval-distances)]
          (if (> (:d collision-point-candidate) (* p-size 1.1))
            nil
            {:c1 (nth line-1-intervals (:i collision-point-candidate)) :c2 (nth line-2-intervals (:i collision-point-candidate))}))))))

(defn get-collision-point-of-contact [a-line-center b-line-center]
  (let [granularity 20
        [[a-start-x a-start-y] [a-end-x a-end-y] a-radius] a-line-center
        [[b-start-x b-start-y] [b-end-x b-end-y] b-radius] b-line-center
        sum-of-radii (+ a-radius b-radius)
        a-distance-x (- a-end-x a-start-x)
        a-distance-y (- a-end-y a-start-y)
        b-distance-x (- b-end-x b-start-x)
        b-distance-y (- b-end-y b-start-y)
        a-interval-size-x (/ a-distance-x granularity)
        a-interval-size-y (/ a-distance-y granularity)
        b-interval-size-x (/ b-distance-x granularity)
        b-interval-size-y (/ b-distance-y granularity)]
    (reduce
     (fn [[a b] granularity-idx]
       (let [ax-interval-movement (* a-interval-size-x granularity-idx)
             ay-interval-movement (* a-interval-size-y granularity-idx)
             bx-interval-movement (* b-interval-size-x granularity-idx)
             by-interval-movement (* b-interval-size-y granularity-idx)
             a-coord [(+ a-start-x ax-interval-movement) (+ a-start-y ay-interval-movement)]
             b-coord [(+ b-start-x bx-interval-movement) (+ b-start-y by-interval-movement)]
             d0 (distance-between-points a b)
             d1 (distance-between-points a-coord b-coord)
             diff-d0-ballsize (- d0 (+ sum-of-radii 0.2))
             diff-d1-ballsize (- d1 (+ sum-of-radii 0.2))
             d0-psize-proximity (Math/abs diff-d0-ballsize)
             d1-psize-proximity (Math/abs diff-d1-ballsize)
             time-perc-of-collision-in-frame (/ granularity-idx granularity)]
         (if (and (< d1-psize-proximity d0-psize-proximity) (>= diff-d1-ballsize 0))
           [a-coord b-coord time-perc-of-collision-in-frame]
           [a b time-perc-of-collision-in-frame])))
     [[a-start-x a-start-y] [b-start-x b-start-y] 0]
     (range (inc granularity)))))

(defn get-collision-point-of-contact-2 [a-line-center b-line-center ball-size]
  (let [[[a-start-x a-start-y] [a-end-x a-end-y]] a-line-center
        [[b-start-x b-start-y] [b-end-x b-end-y]] b-line-center
        a-distance-x (- a-end-x a-start-x)
        a-distance-y (- a-end-y a-start-y)
        b-distance-x (- b-end-x b-start-x)
        b-distance-y (- b-end-y b-start-y)
        diff-starts-x (- b-start-x a-start-x)
        diff-distances-x (- b-distance-x a-distance-x)
        diff-starts-y (- b-start-y a-start-y)
        diff-distances-y (- b-distance-y a-distance-y)
        double-diff-distances-of-diff-starts-x (* 2 diff-starts-x diff-distances-x)
        double-diff-distances-of-diff-starts-y (* 2 diff-starts-y diff-distances-y)
        sum-of-square-diff-starts (+ (Math/pow diff-starts-x 2) (Math/pow diff-starts-y 2)) ;; a
        sum-of-double-diff-distances-of-diff-starts (+ double-diff-distances-of-diff-starts-x double-diff-distances-of-diff-starts-y) ;; b
        sum-of-square-diff-distances (- (+ (Math/pow diff-distances-x 2) (Math/pow diff-distances-y 2)) ball-size) ;; c
        quadratic-calculation (Math/sqrt (- (Math/pow sum-of-double-diff-distances-of-diff-starts 2) (* 4 sum-of-square-diff-starts sum-of-square-diff-distances)))
        possible-times-of-collision-quadratic (filterv  #(>= % 0) [(/ (- 0 sum-of-double-diff-distances-of-diff-starts quadratic-calculation) (* 2 sum-of-square-diff-starts))
                                                                                  (/ (+ (- 0 sum-of-double-diff-distances-of-diff-starts) quadratic-calculation) (* 2 sum-of-square-diff-starts))])
        possible-times-exist (not= 0 (count possible-times-of-collision-quadratic))
        collision-time (if possible-times-exist (apply min possible-times-of-collision-quadratic) nil)
        a-point-of-collision (if possible-times-exist [(+ a-start-x (* a-distance-x collision-time)) (+ a-start-y (* a-distance-y collision-time))] nil)
        b-point-of-collision (if possible-times-exist [(+ b-start-x (* b-distance-x collision-time)) (+ b-start-y (* b-distance-y collision-time))] nil)]
    [a-point-of-collision b-point-of-collision collision-time]))