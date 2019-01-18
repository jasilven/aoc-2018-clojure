(ns day23
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn fname->nbots [fname]
  (let [lines (str/split-lines (str/trim (slurp fname)))]
    (for [line lines]
      (->> line
           (re-seq #"-?\d+")
           (map #(Integer/parseInt %))
           (into [])))))

(defn strongest [nbots]
  (apply max-key last nbots))

(defn distance [xyz1 xyz2]
  (reduce + (map #(Math/abs (- %2 %1)) (take 3 xyz1) (take 3 xyz2))))

(defn solve1 [fname]
  (let [nbots (fname->nbots fname)
        [_ _ _ r] (strongest nbots)]
    (count (filter #(<= (distance % (strongest nbots)) r) nbots))))

(defn third [coll]
  (nth coll 2))

(defn find-area-limits [nbots]
  (vector (apply min (map first nbots))
          (apply min (map second nbots))
          (apply min (map third nbots))
          (apply max (map first nbots))
          (apply max (map second nbots))
          (apply max (map third nbots))))

(defn nbots-in-range [[_ _ _ r :as origin] nbots]
  (filter #(< (distance % origin) (+ r (last %))) nbots))

(defn find-target-grid [x1 y1 z1 x2 y2 z2 grid-size nbots]
  (->> (for [x (range x1 (inc x2) grid-size)
             y (range y1 (inc y2) grid-size)
             z (range z1 (inc z2) grid-size)]
         (vector [x y z] (count (nbots-in-range [x y z grid-size] nbots))))
       (reduce #(cond
                  (> (second %2) (second (first %1))) (vector %2)
                  (= (second %2) (second (first %1))) (conj %1 %2)
                  :else %1)
               (vector [[nil nil nil] 0]))
       (apply min-key #(distance [0 0 0] (first %)))))

(defn solve2 [fname]
  (let [nbots (fname->nbots fname)
        [x1 y1 z1 x2 y2 z2] (find-area-limits nbots)]
    (loop [x1 x1
           y1 y1
           z1 z1
           x2 x2
           y2 y2
           z2 z2
           grid-size (- x2 x1)]
      (if (< grid-size 1)
        (distance [0 0 0] (into [] (map inc [x1 y1 z1])))
        (let [[[x y z] _] (find-target-grid x1 y1 z1 x2 y2 z2 grid-size nbots)]
          (recur (- x (inc (int (/ grid-size 2))))
                 (- y (inc (int (/ grid-size 2))))
                 (- z (inc (int (/ grid-size 2))))
                 (+ x (inc (int (/ grid-size 2))))
                 (+ y (inc (int (/ grid-size 2))))
                 (+ z (inc (int (/ grid-size 2))))
                 (int (/ grid-size 2))))))))
(comment
  (solve1 "resources/day23-input.txt")
  ;; correct answer 240
  (solve2 "resources/day23-input.txt")
  ;; correct answer 116547949
  )

(defn -main []
  (println "part 1:" (solve1 "resources/day23-input.txt"))
  (println "part 2:" (solve2 "resources/day23-input.txt")))
