(ns day25
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn fname->costellations [fname]
  (->> (for [line (str/split-lines (str/trim (slurp fname)))]
         (mapv #(Integer/parseInt %) (re-seq #"-?\d+" line)))
       (map #(hash-set %))
       (apply conj #{})))

(defn mdist [p1 p2]
  (reduce + (map #(Math/abs (- %2 %1)) p1 p2)))

(defn mdist-costellations [c1 c2]
  (->> (for [a c1 b c2]
         (mdist a b))
       (apply min)))

(defn can-join? [c1 c2]
  (if (<= (mdist-costellations c1 c2) 3) true false))

(defn joinable-costellations [target costellations]
  (->> (disj costellations target)
       (filter #(can-join? target %))))

(defn join-costellations [costellations]
  (loop [unvisited costellations
         result #{}]
    (if (empty? unvisited)
      result
      (let [current (first unvisited)
            joinable (joinable-costellations current costellations)]
        (recur (apply disj unvisited (conj joinable current))
               (conj result (set/union current (apply set/union joinable))))))))

(defn solve1 [fname]
  (->> (fname->costellations fname)
       (iterate join-costellations)
       (reduce #(if (not= %2 %1) %2 (reduced %1)))
       count))

(comment
  (solve1 "resources/day25-input.txt")
  ;; correct answer 388
  )

(defn -main []
  (println "part 1:" (solve1 "resources/day25-input.txt")))
