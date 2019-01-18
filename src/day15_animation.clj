(ns day15-animation
  (:require [day15]
            [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  (let [board (day15/file->board "resources/day15-input.txt")
        item-size 20
        w (* item-size (inc (apply max (map first (keys board)))))
        h (* item-size (inc (apply max (map second (keys board)))))
        units (map first (day15/all-units board))]
    (q/background 0)
    (q/frame-rate 3)
    (q/color-mode :rgb)
    (q/resize-sketch w h)
    {:board board
     :hps (into {} (mapv #(vector % 200) units))
     :epower 3
     :item-size item-size
     :round 0}))

(defn draw [{:keys [board round item-size]}]
  (doseq [[[x y] ch] (seq board)]
    (q/fill 255 255 255)
    (q/text (str "round: " round) 5 20)
    (q/fill 0 0 250)
    (q/text "elves" 5 40)
    (q/fill 200 0 0)
    (q/text "goblins" 5 60)
    (case ch
      \# (q/fill 0 0 0)
      \G (q/fill 200 0 0)
      \E (q/fill 0 0 250)
      (q/fill 200 200 200))
    (q/rect (* item-size x) (* item-size y) item-size item-size)))

(defn -main [& args]
  (q/defsketch day15
    :host "host"
    :title "Advent of code 2018 - day 15"
    :size [300 300]
    :setup setup
    :draw draw
    :key-pressed (fn [state _] (if (q/looping?) (q/no-loop) (q/start-loop)) state)
    :middleware [m/fun-mode]
    :update day15/play))
