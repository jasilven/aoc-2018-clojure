(ns day10-animation
  (:require [day10]
            [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  (let [points (day10/input->data "resources/day10-input.txt")]
    (q/background 0)
    (q/frame-rate 30)
    (q/color-mode :rgb)
    (first (drop 10400 (iterate day10/move-points (assoc {}
                                                         :points points
                                                         :secs 0
                                                         :prev-limits nil
                                                         :prev-points nil
                                                         :limits (day10/calc-limits points)))))))

(defn draw [{:keys [points secs limits] :as state}]
  (q/background 0)
  (doseq [[x y] (seq points)]
    (q/stroke-weight 3)
    (q/stroke 255 250 205)
    (q/point (+ 265 x) (+ 225 y))
    (q/fill 255 255 255)
    (q/text (str "secs:" secs) 5 20)))

(defn update-state [{:keys [points secs limits] :as state}]
  (let [ps (day10/update-points points)]
    (if (= secs 10867)
      state
      (assoc state
             :points ps
             :secs (inc secs)
             :prev-limits limits
             :prev-points points
             :limits (day10/calc-limits ps)))))

(defn -main [& args]
  (q/defsketch day10
    :host "host"
    :title "Advent of code 2018 - day 10"
    :size [1000 800]
    :setup setup
    :draw draw
    :key-pressed (fn [state _] (if (q/looping?) (q/no-loop) (q/start-loop)) state)
    :middleware [m/fun-mode]
    :update update-state))
