(ns day2
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.data :as d]))

(def input (str/split-lines (slurp "resources/day2-input.txt")))

(defn count-twos-threes [ids]
  (for [id ids]
    (let [freqs (frequencies (str/split id #""))
          twos (filter #(= 2 (val %)) freqs)
          threes (filter #(= 3 (val %)) freqs)]
      [(if (< 0 (count twos)) 1 0)
       (if (< 0 (count threes)) 1 0)])))

(defn solve1 [ids]
  (let [counts (count-twos-threes ids)
        sum-twos (reduce + (map first counts))
        sum-threes (reduce + (map second counts))]
    (* sum-twos sum-threes)))

(defn match? [s1 s2]
  (let [things-in-both (last (d/diff (into [] s1) (into [] s2)))
        same-chars (filter some? things-in-both)]
    (when (= (count s1)
             (inc (count same-chars)))
      (apply str same-chars))))

(defn solve2 [ids]
  (->> (combo/combinations ids 2)
       (map #(match? (first %) (second %)))
       (drop-while nil?)
       first))

(comment
  (solve1 input)
  ;; correct answer 8892
  (solve2 input)
  ;; correct answer "zihwtxagifpbsnwleydukjmqv"
  )

(defn -main [& args]
  (println "part 1:" (solve1 input))
  (println "part 2:" (solve2 input)))
