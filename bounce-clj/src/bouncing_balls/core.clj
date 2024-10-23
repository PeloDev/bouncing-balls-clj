(ns bouncing-balls.core
  (:import [javax.swing JFrame JPanel Timer]
           [java.awt Graphics Graphics2D Color RenderingHints]
           [java.awt.event ActionListener]
           )
  (:require [bouncing-balls.data.config.regular :refer :all]
            [bouncing-balls.state-fns.regular :refer :all]))

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

(defn update-state [states]
  (->> states
       (mapv apply-move)
       (apply-collisions)
       (mapv apply-boundary-bounce)
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
        new-frame (doto (JFrame. (str "Bouncing Balls!"))
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
