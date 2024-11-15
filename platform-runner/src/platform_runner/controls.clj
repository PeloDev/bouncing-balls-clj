(ns platform-runner.controls)

(def temp-movement-speed 8)

(defn move-x [player direction]
  (assoc player :moving-x (direction 0 temp-movement-speed)))

(defn stop-x [player]
  (assoc player :moving-x 0))

(defn move-y [player direction]
  (assoc player :moving-y (direction 0 temp-movement-speed)))

(defn stop-y [player]
  (assoc player :moving-y 0))

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

(defn release-control-player [player key-code]
  (condp = key-code
    java.awt.event.KeyEvent/VK_LEFT (stop-x player)
    java.awt.event.KeyEvent/VK_RIGHT (stop-x player)
    java.awt.event.KeyEvent/VK_UP player
    java.awt.event.KeyEvent/VK_DOWN player
    java.awt.event.KeyEvent/VK_SPACE player
    java.awt.event.KeyEvent/VK_ENTER player
    java.awt.event.KeyEvent/VK_BACK_SPACE player
    java.awt.event.KeyEvent/VK_TAB player
    player)
  ;; TODO...
  )