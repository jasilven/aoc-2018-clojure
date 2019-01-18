(ns day17-animation2
  (:require [day17]
            [quil.core :as q]
            [quil.middleware :as m]
            [clojure.set :as set]))

(defn draw [{:keys [clay water surface maxy w h ypos] :as state}]
  (let [water-color (q/color 111 168 220)]
    (doseq [[x y] clay]
      (q/set-pixel x (+ y ypos) (q/color 250 0 0)))
    (doseq [x (range w)]
      (q/set-pixel x (+ maxy ypos) water-color))
    (doseq [[x y] (remove #(day17/solo? % (set/difference water surface) clay)
                          (set/difference water surface))]
      (q/set-pixel x (+ y ypos) water-color))
    (q/fill 255 255 255)
    (q/text (str "water: " (day17/part2 state)) 5 20)))

(defn setup []
  (let [initial (day17/initial-state "resources/day17-input.txt")
        state (first (drop-while #(not (% :done)) (iterate day17/flow initial)))
        clay (state :clay)
        maxy (state :maxy)
        minx (apply min (map first clay))
        maxx (apply max (map first clay))
        w (inc (apply max (map first clay)))
        h (apply max (map second clay))
        ypos 0]
    (q/frame-rate 0.3)
    (q/background 0)
    (q/resize-sketch w (min 1270 h))
    (assoc state :maxy maxy :w w :h h :ypos ypos)))

(defn update-state [state] state)

(defn -main []
  (q/defsketch day17-part2
    :title "Advent of code 2018 - day 17, part 2"
    :setup setup
    :draw draw
    :middleware [m/fun-mode]
    :update update-state))
