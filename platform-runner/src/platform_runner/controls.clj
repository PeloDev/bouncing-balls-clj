(ns platform-runner.controls)

(defn control-player [player key-code]
  (condp = key-code
    java.awt.event.KeyEvent/VK_LEFT "Left"
    java.awt.event.KeyEvent/VK_RIGHT "Right"
    java.awt.event.KeyEvent/VK_UP "Up"
    java.awt.event.KeyEvent/VK_DOWN "Down"
    java.awt.event.KeyEvent/VK_SPACE "Space"
    java.awt.event.KeyEvent/VK_ENTER "Enter"
    java.awt.event.KeyEvent/VK_BACK_SPACE "Back"
    java.awt.event.KeyEvent/VK_TAB "Tab"
    player)
  ;; TODO...
  )