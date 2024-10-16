(ns bouncing-balls.core
  (:import [javax.swing JFrame JPanel Timer]
           [java.awt Graphics Graphics2D Color RenderingHints]
           [java.awt.event ActionListener]
           [java.util Random])
  (:require [bouncing-balls.utils.general :refer :all]
            [bouncing-balls.utils.math :refer :all]
            [bouncing-balls.utils.procedures :refer :all]
            [bouncing-balls.utils.vectors :refer :all]
            [bouncing-balls.physics.fns :refer :all]))

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

(defn get-x-y-from-state [state]
  (mapv state [:x :y]))

(defn get-x-y-from-state-transition [[prev-state next-state]]
  [(get-x-y-from-state prev-state) (get-x-y-from-state next-state)])

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
        indexed-intersections (indexed-pairwise-combination transition-coords (fn [tl-line-one tl-line-two] (find-simple-circle-collision tl-line-one tl-line-two particle-size)) 0)
        collision-index-matrix (get-indexed-pairwise-combination-matrix indexed-intersections (count state-transition))
        new-state (mapv
                   (fn [state-row collision-indexes]
                     (if (or (> (:ghost-frames state-row) 0) (empty? collision-indexes))
                       (assoc state-row :ghost-frames (min 0 (dec (:ghost-frames state-row))))
                       (let [number-of-collisions (count collision-indexes)
                             avg-colliding-state (reduce
                                                  (fn [avg-state idx]
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
                                          ;; are-colliding (or
                                          ;;                end-up-touching
                                          ;;                (and start-off-touching are-converging-or-parallel))
                                          are-colliding (and are-converging-or-parallel (or start-off-touching end-up-touching))]
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
            filtered-collision-point-data (first (filterv #(and (not= nil %)) collision-point-data))]
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
                   :y-velocity (move-towards-zero (- c-y-vel-para o-y-vel-perp) bounce-velocity-loss))))))))

(defn apply-collisions [state-transition]
  (let [;; -----
        [prev-states] (transpose state-transition)
        states-count (count state-transition)
        current-rest-state-pairs (mapv #(extract-nth % state-transition) (range states-count))
        new-states (mapv #(apply bounce-ball %) current-rest-state-pairs)]
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
  (find-collision [[4 8] [8 4]] [[3 3] [10 10]] particle-size)
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