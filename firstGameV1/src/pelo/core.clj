(ns pelo.core
  (:import [javax.swing JFrame JPanel Timer]
           [java.awt Graphics Graphics2D Color RenderingHints]
           [java.awt.event ActionListener]
           [java.util Random])
  (:require [pelo.helpers :refer :all]))

(def frames-per-second 30)
(def ticks-per-second (/ 1000 frames-per-second))
(def particle-count 8)
(def particle-size 24)
(def initial-velocity 2) ;; px per tick
(def gravitational-force 0.04)
(def max-x (- 800 particle-size))
(def min-x 0)
(def max-y (- 600 particle-size))
(def min-y 0)
(defn random-value [upper-bound] (.nextInt (Random.) upper-bound))
(defn random-pos-neg [x] (if (= 1 (random-value 2)) x (* x -1)))
(def bounce-velocity-loss 0.1)

(def frame (atom nil)) ; Atom to store the frame

(def states (atom
             (vec (map
                   (fn [_]
                     {:x (random-value 599)
                      :y (random-value 599)
                      :angle (random-value 360)
                      :x-velocity nil
                      :y-velocity nil
                      :colour Color/WHITE})
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

(defn get-linear-function [[[x1 y1] [x2 y2]]]
  (let [m (/ (- y2 y1) (- x2 x1))
        c (- y2 (* m x2))] (fn [x] (+ (* x m) c))))

(defn list-contains-value [val list]
  (not= nil (some #(= val %) list)))

(defn get-line-vars [[[x1 y1] [x2 y2]]]
  (let [m (safe-divide (- y2 y1) (- x2 x1))
        c (if (nil? m) nil (- y2 (* m x2)))]
    {:m m :c c}))

(defn distance-between-points [point-1 point-2]
  (let [[x1 y1] point-1
        [x2 y2] point-2
        diff-x (- x2 x1)
        diff-y (- y2 y1)]
    (Math/sqrt (+ (* diff-x diff-x) (* diff-y diff-y)))))

;; This is great, but we're not looking for a intersection at the scale of a single pixel,
;; but an area
(defn find-intersection [line-one line-two]
  (let [{m1 :m c1 :c} (get-line-vars line-one)
        {m2 :m c2 :c} (get-line-vars line-two)
        vars-list-has-nil (list-contains-value nil '(m1 m2 c1 c2))
        x-intercept (if vars-list-has-nil nil (safe-divide (- c2 c1) (- m1 m2)))
        y-intercept (if (nil? x-intercept) nil ((get-linear-function line-one) x-intercept))
        line-one-xs-list (apply list (mapv first line-one))
        min-max-xs-one (list (apply min line-one-xs-list) (apply max line-one-xs-list))
        is-within-range (and
                         (not= x-intercept nil)
                         (apply < (interpose x-intercept min-max-xs-one)))]
    (if is-within-range {:x x-intercept :y y-intercept} nil)))

(defn find-collision [line-one line-two]
  (let [p-intersection (find-intersection line-one line-two)]
    (if (nil? p-intersection)
      nil
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

;; another naive approach:
;; - what about if the cross paths within the same y range, meaning lines don't intersect and their final states aren't touching (therefore no collision...)
(defn find-point-of-contact [[tlx1 tly1] [tlx2 tly2] p-size]
  (let [centre-one (get-center-point-from-top-left [tlx1 tly1] p-size)
        centre-two (get-center-point-from-top-left [tlx2 tly2] p-size)
        distance-between-centers (distance-between-points centre-one centre-two)
        are-boxes-touching (<= distance-between-centers p-size)]
    (if are-boxes-touching
      {:x (get-middle-of-two-numbers (nth centre-one 0) (nth centre-two 0))
       :y (get-middle-of-two-numbers (nth centre-one 1) (nth centre-two 1))}
      nil)))

;; quite naive but can be improved on in future:
;; - objects that cross paths in one transition may not collide due to different velocities
;; - doesn't account for all corners of box (even later in future the perimeter of circle)
(defn find-boxlike-intersection [tl-line-one tl-line-two]
  (let [br-line-one (mapv (fn [tl-point] (get-bottom-right-point-from-top-left tl-point particle-size)) tl-line-one)
        br-line-two (mapv (fn [tl-point] (get-bottom-right-point-from-top-left tl-point particle-size)) tl-line-two)
        point-of-contact (find-point-of-contact (last tl-line-one) (last tl-line-two) particle-size)]
    (if (nil? point-of-contact)
      (first
       (filterv #(not= nil %) [(find-collision tl-line-one tl-line-two)
                               (find-collision tl-line-one br-line-two)
                               (find-collision br-line-one tl-line-two)
                               (find-collision br-line-one br-line-two)]))
      point-of-contact)))

(defn determine-is-collision-between-paths [tl-line-one tl-line-two]
  (let [br-line-one (mapv (fn [tl-point] (get-bottom-right-point-from-top-left tl-point particle-size)) tl-line-one)
        br-line-two (mapv (fn [tl-point] (get-bottom-right-point-from-top-left tl-point particle-size)) tl-line-two)
        point-of-contact (find-point-of-contact (last tl-line-one) (last tl-line-two) particle-size)]
    (if (nil? point-of-contact)
      (= true
         (some true? [(determine-is-collision tl-line-one tl-line-two)
                      (determine-is-collision tl-line-one br-line-two)
                      (determine-is-collision br-line-one tl-line-two)
                      (determine-is-collision br-line-one br-line-two)]))
      true)))


(comment
  (def bmtemp (apply list (mapv first [[5 1] [2 3]])))
  (interpose 3 bmtemp)
  (apply > (interpose 10 bmtemp))
  (apply min  bmtemp)

  (defn test-my-fn []
    (let [my-fn (fn [n] (* n n 2))]
      (my-fn 8)))

  (test-my-fn)


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
                            test-intersections (indexed-pairwise-combination test-state-x-ys determine-is-collision-between-paths 0)
                            consolidated-test-intersections (consolidate-indexed-pairwise-combination-result test-intersections (count test-state-transition))
                            consolidated-test-intersections-matrix (get-indexed-pairwise-combination-matrix test-intersections (count test-state-transition))
                            test-collisions (apply-collisions test-state-transition)]
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
  ; [tlx1 tly1] [386.9021130325903 243.6580339887499]
; [brx1 bry1] [391.9021130325903 248.6580339887499]
; [tlx2 tly2] [81.2036300463041 547.6372710200945]
; [brx2 bry2] [86.2036300463041 552.6372710200945]
; [tlx1 tly1] [386.9021130325903 243.6580339887499]
; [brx1 bry1] [391.9021130325903 248.6580339887499]
; [tlx2 tly2] [81.2036300463041 547.6372710200945]
; [brx2 bry2] [86.2036300463041 552.6372710200945]
  (find-point-of-contact
   [[386.9021130325903 243.6580339887499] [391.9021130325903 248.6580339887499]]
   [[81.2036300463041 547.6372710200945] [86.2036300463041 552.6372710200945]]
   particle-size)
  (get-pairwise-combination-indexes 5)
  (def my-init)
  (def my-map {:a 3 :b 4 :c "4"})
  (:a my-map)
  (contains? [1 2 3 4] 2)
  ;; (def test-trans-intersections (mapv find-intersection test-state-x-ys))
  ;; END
  )


;; adding to 
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
    ;; (println (str "Change in V" (- new-y-velocity (if (nil? y-velocity) 0 y-velocity))))
    ;; [state {:x new-x :y new-y :angle angle :x-velocity x-velocity :y-velocity new-y-velocity}]
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
        ;; bounce-x-velocity (if (and is-x-bounce (not= nil new-x-velocity)) (- new-x-velocity bounce-velocity-loss) new-x-velocity)
        bounce-x-velocity (if is-x-bounce (+ (- 0 new-x-velocity) bounce-velocity-loss) new-x-velocity)
        bounce-y-velocity (if is-y-bounce (+ (- 0 new-y-velocity) bounce-velocity-loss) new-y-velocity)
        next-state (assoc new-state :x bounce-x :y bounce-y :x-velocity bounce-x-velocity :y-velocity bounce-y-velocity :angle bounce-angle)]
        ;; .....

    ;; [new-state {:x bounce-x :y bounce-y :x-velocity bounce-x-velocity :y-velocity bounce-y-velocity :angle bounce-angle}]
    [new-state next-state]))

(defn debug-paint-red-intersection [state-transition intersection-data]
  (let [[prev-state next-state] state-transition
        updated-next-state (if (not= (some identity intersection-data) nil)
                             (assoc next-state :colour Color/RED)
                             (assoc next-state :colour Color/WHITE))]
    [prev-state updated-next-state]))

(defn apply-collisions [state-transition]
  (let [;; -----
        [prev-states next-states] (transpose state-transition)
        transition-coords (mapv get-x-y-from-state-transition state-transition)
        indexed-intersections (indexed-pairwise-combination transition-coords determine-is-collision-between-paths 0)
        collision-index-matrix (get-indexed-pairwise-combination-matrix indexed-intersections (count state-transition))
        new-state (mapv
                   (fn [state-row collision-indexes]
                     (if (empty? collision-indexes)
                       state-row
                       (let [colliding-states-data (mapv (fn [idx]
                                                           (select-keys (nth next-states idx) [:x :y :x-velocity :y-velocity]))
                                                         collision-indexes)
                             number-of-collisions (count collision-indexes)
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
                                                  collision-indexes)
                             calc-new-pos (fn [k-pos k-vel ref other]
                                            ;; (println "k: " k ", ref: " ref ", other: " other)
                                            (let [ref-future-pos (+ (k-pos ref) 0);; TODO: replace 0 with `(k-vel ref)` or revert
                                                  other-future-pos (+ (k-pos other) 0);; TODO: replace 0 with `(k-vel other)` or revert
                                                  diff-future-pos (- ref-future-pos other-future-pos)
                                                  abs-diff-future-pos (Math/abs diff-future-pos)]
                                              (if (>= abs-diff-future-pos 5)
                                                ref-future-pos
                                                (+ ref-future-pos (* (/ diff-future-pos abs-diff-future-pos) (- particle-size abs-diff-future-pos))))))]
                         (assoc state-row
                                :x (calc-new-pos :x :x-velocity state-row avg-colliding-state)
                                :y (calc-new-pos :y :y-velocity state-row avg-colliding-state)
                                :x-velocity (- (+ 0 (:x-velocity avg-colliding-state)) bounce-velocity-loss);; TODO: replace 0 with `(:x-velocity state-row)` or revert
                                :y-velocity (- (+ 0 (:y-velocity avg-colliding-state)) bounce-velocity-loss);; TODO: replace 0 with `(:y-velocity state-row)` or revert
                                                                      ;; :x-velocity (-
                                                                      ;;              (average-mean
                                                                      ;;               (mapv (fn [idx]
                                                                      ;;                       (:x-velocity (nth next-states idx)))
                                                                      ;;                     collision-indexes))
                                                                      ;;              bounce-velocity-loss)
                                                                      ;; :y-velocity (-
                                                                      ;;              (average-mean
                                                                      ;;               (mapv (fn [idx]
                                                                      ;;                       (:y-velocity (nth next-states idx)))
                                                                      ;;                     collision-indexes))
                                                                      ;;              bounce-velocity-loss)
                                ))))
                   next-states
                   collision-index-matrix)]
    (transpose [prev-states new-state])))

(defn update-state [states]
  (->> states
       (mapv apply-move)
       (apply-collisions)
       (mapv apply-bounce)
       (mapv last)))
(-> ())
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

  