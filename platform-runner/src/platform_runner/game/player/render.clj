(ns platform-runner.game.player.render
  (:import [javax.imageio ImageIO]
           [java.io File]))

(defn load-image [file-path]
  (ImageIO/read (File. file-path)))

(defn draw-player-character [g player]
  (let [player-image (load-image "resources/images/basic_stickman_w.png")]
    (.drawImage g player-image (:x player) (:y player) 60 60 nil))
  )