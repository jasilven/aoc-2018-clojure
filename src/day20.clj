(ns day20
  (:require [clojure.string :as str]))

(defonce initial-input (rest (butlast (str/trim (slurp "resources/day20-input.txt")))))

(def room-move {\N [0 -2] \E [2 0] \S [0 2] \W [-2 0]})
(def door-move {\N [0 -1] \E [1 0] \S [0 1] \W [-1 0]})

(defn walk [input stack prevpos curpos rooms doors]
  (if (empty? input)
    {:rooms rooms :doors doors}
    (case (first input)
      (\N \E \W \S) (let [newpos (map + (room-move (first input)) curpos)]
                      (recur (rest input)
                             stack
                             curpos newpos
                             (assoc rooms
                                    newpos (min (get rooms newpos (Integer/MAX_VALUE))
                                                (inc (rooms curpos))))
                             (conj doors (map + (door-move (first input)) curpos))))
      \( (recur (rest input)
                (conj stack curpos)
                curpos curpos
                rooms doors)
      \| (recur (rest input)
                stack
                (first stack) (first stack)
                rooms doors)
      \) (recur (rest input)
                (rest stack)
                (first stack) (first stack)
                rooms doors))))

(defn solve1 [input]
  (->> (walk (for [ch input] ch)
             (list)
             [0 0] [0 0]
             {[0 0] 0}
             #{})
       (:rooms)
       vals
       (apply max)))

(defn solve2 [input]
  (->> (walk (for [ch input] ch)
             (list)
             [0 0] [0 0]
             {[0 0] 0}
             #{})
       (:rooms)
       vals
       (filter #(>= % 1000))
       count))

(comment
  (solve1 initial-input)
  ;; correct answer 4308
  (solve2 initial-input)
  ;; correct answer 8528
  )

(defn -main []
  (println "part 1:" (solve1 initial-input))
  (println "part 2:" (solve2 initial-input)))
