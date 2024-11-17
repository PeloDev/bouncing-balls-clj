(ns platform-runner.game.player.render
  (:import [javax.imageio ImageIO]
           [java.io File]
           [java.awt Graphics2D Color RenderingHints]))

(defn load-image [file-path]
  (ImageIO/read (File. file-path)))

(defn draw-player-character [g {:keys [x y]}]
  (let [player-image (load-image "resources/images/basic_stickman_w.png")
        g2d (doto ^Graphics2D g
              (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON))]
    (.drawImage g player-image x y 60 60 nil)
    ;; Draw debug border
    (.setColor g2d Color/RED)
    (.drawRect g2d x y 60 60)))