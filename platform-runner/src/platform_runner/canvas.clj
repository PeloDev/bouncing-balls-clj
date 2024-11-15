(ns platform-runner.canvas
  (:import [javax.swing JFrame JPanel Timer]
           [java.awt Graphics Graphics2D Color RenderingHints]
           [java.awt.event ActionListener]
           [javax.imageio ImageIO]
           [java.io File])
  (:require [platform-runner.config :refer [config]]
            [platform-runner.state :refer [player update-player]]
            [platform-runner.controls :refer [control-player release-control-player]]))

(def frame (atom nil)) ; Atom to store the frame
(def ticks-per-second (/ 1000 60))
(def canvas-x-boundary (:x-range (:viewport config)))
(def canvas-y-boundary (:y-range (:viewport config)))

(defn load-image [file-path]
  (ImageIO/read (File. file-path)))

(defn draw-player-character [g player-image player]
  (.drawImage g player-image (:x player) (:y player) 60 60 nil))

(defn draw-canvas [^Graphics g]
  (let [g2d (doto ^Graphics2D g
              (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON))]
    (.setColor g2d Color/BLACK)
    (.fillRect g2d (first canvas-x-boundary) (first canvas-y-boundary) (last canvas-x-boundary) (last canvas-y-boundary))))

(defn game-panel []
  (let [player-image (load-image "resources/images/basic_stickman_w.png")]
    (proxy [JPanel ActionListener] []
      (paintComponent [^Graphics g]
        (proxy-super paintComponent g)
        (draw-canvas g)
        (draw-player-character g player-image @player))
      (actionPerformed [_]
        (swap! player update-player)
        (.repaint this)))))

(defn create-frame []
  (when-let [f @frame]
    (.dispose f) ; Close the existing frame if it exists
    (reset! frame nil))  ; Reset the frame atom 
  (let [panel (game-panel)
        new-frame (doto (JFrame. (str "Platform Runner!"))
                    (.setContentPane panel)
                    (.setSize (last canvas-x-boundary) (+ 40 (last canvas-y-boundary)))
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
                                      (.getKeyChar e)))
                           (swap! player control-player key-code)))
                       (keyReleased [this e]
                         (swap! player release-control-player (.getKeyCode e)))
                       (keyTyped [this e])))
    (.start timer)
    ;; (.start log-timer)
    (reset! frame new-frame))) ; Store the new frame in the atom

