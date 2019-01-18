(ns day18-animation
  (:require [day18]
            [quil.core :as q]
            [quil.middleware :as m]))

(defn update-wh [{:keys [w h acremap] :as all}]
  (assoc all
         :w (count (distinct (map first (keys acremap))))
         :h (count (distinct (map second (keys acremap))))))

(defn setup []
  (let [rect-size 8
        state (update-wh {:acremap (day18/input->acremap "resources/day18-input.txt")
                          :rect-size rect-size
                          :total 0
                          :part1 nil
                          :minute 0})]
    (q/background 0 0 0)
    (q/frame-rate 60)
    (q/color-mode :rgb)
    (q/resize-sketch (* rect-size (state :w)) (* rect-size (state :h)))
    state))

(defn draw [{:keys [w h rect-size acremap minute total part1]}]
  (q/stroke 0 0 0)
  (doseq [y (range h)]
    (doseq [x (range w)]
      (case (acremap [x y])
        \. (q/fill 0 0 0)
        \| (q/fill 0 200 0)
        \# (q/fill 200 0 0))
      (q/rect (* x rect-size) (* y rect-size) rect-size rect-size)))
  (q/fill 255 255 255)
  (q/text (str "minute: " minute) 5 20)
  (q/text (str "part 1: " part1) 5 40)
  (q/text (str "part 2: " total) 5 60)
  (when part1
    (q/text (str "There is of course faster way to solve part 2!") 5 80)))

(defn update-state [{:keys [minute total] :as all}]
  (let [all (if (= 10 minute) (assoc all :part1 total) all)]
    (if (< minute 1000000000)
      (day18/transform-acremap all)
      all)))

(defn -main [& args]
  (q/defsketch day18
    :title "Advent of code 2018 - day 18"
    :size [10 10]
    :setup setup
    :draw draw
    :key-pressed (fn [state _] (if (q/looping?) (q/no-loop) (q/start-loop)) state)
    :middleware [m/fun-mode]
    :update update-state))
