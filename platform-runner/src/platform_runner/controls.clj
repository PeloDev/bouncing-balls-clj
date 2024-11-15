(ns platform-runner.controls)

(def temp-movement-speed 10)

(defn move-x [player direction]
  (assoc player :x (direction (:x player) temp-movement-speed)))

(defn move-y [player direction]
  (assoc player :y (direction (:y player) temp-movement-speed)))

(defn control-player [player key-code]
  (condp = key-code
    java.awt.event.KeyEvent/VK_LEFT (move-x player -)
    java.awt.event.KeyEvent/VK_RIGHT (move-x player +)
    java.awt.event.KeyEvent/VK_UP player
    java.awt.event.KeyEvent/VK_DOWN player
    java.awt.event.KeyEvent/VK_SPACE player
    java.awt.event.KeyEvent/VK_ENTER player
    java.awt.event.KeyEvent/VK_BACK_SPACE player
    java.awt.event.KeyEvent/VK_TAB player
    player)
  ;; TODO...
  )