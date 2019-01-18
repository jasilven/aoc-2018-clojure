(ns day17-animation
  (:require [day17]
            [quil.core :as q]
            [quil.middleware :as m]))

(defn draw [{:keys [done clay point img minx w h water surface down edges] :as all}]
  (let [[x y] point
        ypos (if (> y 1000) -501 0)
        water-color (q/color 111 168 220)]
    (q/image img 0 ypos)
    (q/fill 255 255 255)
    (q/stroke 138 173 184)
    (q/stroke-weight 5)
    (q/point 500 2)
    (q/text (str "coords: x:" x ", y:" (+ y ypos)) 5 40)
    ;; draw water
    (doseq [[x y] water]
      (q/set-pixel x (+ y ypos) water-color))
    ;; draw surface
    (doseq [[x y] surface]
      (q/set-pixel x (+ y ypos) water-color))
    ;; draw down
    (doseq [[x y] down]
      (q/set-pixel x (+ y ypos) water-color))
    ;; draw edges
    (doseq [[x y] edges]
      (q/stroke 200 200 0)
      (q/stroke-weight 5)
      (q/point x (+ y ypos)))
    (if (not done)
      (do
        ;; draw pointer line
        (q/stroke 40 140 40)
        (q/stroke-weight 1)
        (q/line minx (+ y ypos) w (+ y ypos))
        ;; draw point
        (q/stroke-weight 4)
        (q/point x (+ y ypos))
        (q/text (str "water: " (count water)) 5 20))
      (q/text (str "water: " (day17/part1 all)) 5 20))))

(defn setup []
  (let [state (day17/initial-state "resources/day17-input.txt")
        clay (state :clay)
        maxy (state :maxy)
        minx (dec (apply min (map first clay)))
        maxx (dec (apply max (map first clay)))
        w (inc (apply max (map first clay)))
        h (inc (apply max (map second clay)))
        img (q/create-image w h :rgb)]
    (dotimes [x w]
      (q/set-pixel img x maxy (q/color 111 168 220))
      (dotimes [y h]
        (when (contains? clay [x y]) (q/set-pixel img x y (q/color 250 0 0)))))
    (q/frame-rate 30)
    (q/color-mode :rgb)
    (q/resize-sketch w (min 1270 h))
    (assoc state :img img :minx minx :w w :h h)))

(defn -main [& args]
  (q/defsketch day17-part1
    :title "Advent of code 2018 - day 17, part 1"
    :size [300 300]
    :setup setup
    :draw draw
    :key-pressed (fn [state _] (if (q/looping?) (q/no-loop) (q/start-loop)) state)
    :middleware [m/fun-mode]
    :update day17/flow))


