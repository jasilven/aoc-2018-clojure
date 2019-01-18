(ns day20-animation
  (:require [day20]
            [quil.core :as q]
            [quil.middleware :as m]
            [clojure.set :as set]))

(defn draw [{:keys [x1 y1 w h pos rooms-doors size] :as state}]
  (let [{:keys [rooms doors]} rooms-doors
        red 180]
    (q/stroke-weight 1)
    (q/stroke red 0 0)
    (q/fill red 0 0)
    (doseq [[x y] (keys rooms)]
      (q/rect (* size (+ 1 x (Math/abs x1)))
              (* size (+ 1 y (Math/abs y1)))
              size size))
    (doseq [[x y] doors]
      (q/rect (* size (+ 1 x (Math/abs x1)))
              (* size (+ 1 y (Math/abs y1)))
              size size))
    (q/stroke 0 200 0)
    (q/fill 0 200 0)
    #_(do
      (q/rect (* size (+ 1 (first pos) (Math/abs x1)))
              (* size (+ 1 (second pos) (Math/abs y1)))
              size size))))

(defn setup []
  (let [rooms-doors (day20/walk (for [ch day20/initial-input] ch)
                                (list)
                                [0 0] [0 0]
                                {[0 0] 0}
                                #{})
        size 5
        x1 (apply min (map first (keys (:rooms rooms-doors))))
        y1 (apply min (map second (keys (:rooms rooms-doors))))
        w (+ (* 3 size) (* size (- (apply max (map first (keys (:rooms rooms-doors)))) x1)))
        h (+ (* 3 size) (* size (- (apply max (map second (keys (:rooms rooms-doors)))) y1)))]
    (q/frame-rate 1)
    (q/background 0)
    (q/resize-sketch w h)
    (assoc {}
           :path (into #{} (set/union (:doors rooms-doors) (keys (:rooms rooms-doors))))
           :pos [0 0] :size size :x1 x1 :y1 y1 :w w :h h :rooms-doors rooms-doors)))

(defn update-state [{:keys [direction pos path rooms-doors] :as state}]
  state
  #_(let [moves (shuffle [(map + (day20/door-move \N) pos)
                        (map + (day20/door-move \E) pos)
                        (map + (day20/door-move \S) pos)
                        (map + (day20/door-move \W) pos)])]
    (assoc state
           :pos (first (filter #(path %) moves)))))

(defn -main []
  (q/defsketch day20
    :title "Advent of code 2018 - day 20"
    :setup setup
    :draw draw
    :middleware [m/fun-mode]
    :update update-state))
