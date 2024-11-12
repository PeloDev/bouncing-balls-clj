(ns platform-runner.canvas
  (:import [javax.swing JFrame JPanel Timer]
           [java.awt Graphics Graphics2D Color RenderingHints]
           [java.awt.event ActionListener]))

(def frame (atom nil)) ; Atom to store the frame
(def ticks-per-second (/ 1000 60))

(defn draw-canvas [^Graphics g]
  (let [g2d (doto ^Graphics2D g
              (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON))]
    (.setColor g2d Color/BLACK)
    (.fillRect g2d 0 0 800 600)))

(defn game-panel []
  (proxy [JPanel ActionListener] []
    (paintComponent [^Graphics g]
      (proxy-super paintComponent g)
      (draw-canvas g))
    (actionPerformed [_]
      (.repaint this))))

(defn create-frame []
  (when-let [f @frame]
    (.dispose f) ; Close the existing frame if it exists
    (reset! frame nil))  ; Reset the frame atom 
  (let [panel (game-panel)
        new-frame (doto (JFrame. (str "Platform Runner!"))
                    (.setContentPane panel)
                    (.setSize 800 640)
                    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
                    (.setVisible true))
        timer (Timer. ticks-per-second panel)]
        ;; ---
    (.addKeyListener new-frame
                     (reify java.awt.event.KeyListener
                       (keyPressed [this e]
                         (let [key-code (.getKeyCode e)]
                           ;; TODO: handle keys to affect game
                           (println "Key pressed:"
                                    (condp = key-code
                                      java.awt.event.KeyEvent/VK_LEFT "Left"
                                      java.awt.event.KeyEvent/VK_RIGHT "Right"
                                      java.awt.event.KeyEvent/VK_UP "Up"
                                      java.awt.event.KeyEvent/VK_DOWN "Down"
                                      java.awt.event.KeyEvent/VK_SPACE "Space"
                                      java.awt.event.KeyEvent/VK_ENTER "Enter"
                                      java.awt.event.KeyEvent/VK_BACK_SPACE "Back"
                                      java.awt.event.KeyEvent/VK_TAB "Tab"
                                      (.getKeyChar e)))))
                       (keyReleased [this e])
                       (keyTyped [this e])))
    (.start timer)
    ;; (.start log-timer)
    (reset! frame new-frame))) ; Store the new frame in the atom

