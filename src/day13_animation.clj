(ns day13-animation
  (:require [day13]
            [quil.core :as q]
            [quil.middleware :as m]))

(defn update-state [{:keys [trackmap carts] :as state}]
  (let [[carts _] (day13/tick trackmap carts)]
    (assoc state :trackmap trackmap :carts carts)))

(defn trackmap-image [trackmap w h]
  (let [img (q/create-image w h :rgb)]
    (q/background 0 0 0)
    (doseq [[x y] (keys trackmap)]
      (q/set-pixel img x y (q/color 240 240 240)))
    img))

(defn draw [{:keys [carts img scale]}]
  (do
    (q/background-image img)
    (q/fill 255 0 0)
    (q/text (str "carts: " (count carts)) 5 20)
    (doseq [[x y] (map #(vector (% :x) (% :y)) (vals carts))]
      (q/stroke-weight 9)
      (q/stroke 199 0 0)
      (q/point (* scale x) (* scale y)))))

(defn setup []
  (let [trackmap (day13/input->trackmap "resources/day13-input.txt")
        carts (day13/trackmap->carts trackmap)
        trackmap (day13/clean-trackmap trackmap)
        scale 4
        mx (apply max (map first (keys trackmap)))
        my (apply max (map second (keys trackmap)))
        img (trackmap-image trackmap mx my)
        w (* scale (inc mx))
        h (* scale (inc my))]
    (q/frame-rate 60)
    (q/color-mode :rgb)
    (q/resize img w h)
    (q/resize-sketch w h)
    {:trackmap trackmap :carts carts :img img :scale scale}))

(defn -main [& args]
  (q/defsketch day13
    :host "host"
    :title "Advent of code 2018 - day 13"
    :size [300 300]
    :setup setup
    :draw draw
    :key-pressed (fn [state _] (if (q/looping?) (q/no-loop) (q/start-loop)) state)
    :middleware [m/fun-mode]
    :update update-state))
