(ns day1
  (:require [clojure.string :as str]))

(defn input-from [fname]
  (for [num (str/split-lines (slurp fname))]
    (Integer/parseInt num)))

(defn solve1 [changes]
  (reduce + changes))

(defn seen?-fn []
  (let [nums (atom #{})]
    (fn [num]
      (if (@nums num)
        true
        (do (swap! nums conj num) false)))))

(defn solve2 [changes]
  (let [freqs (reductions + 0 (cycle changes))
        seen? (seen?-fn)]
    (first (drop-while (complement seen?) freqs))))

(comment
  (solve1 (input-from "resources/day1-input.txt"))
  ;; correct answer: 490
  (solve2 (input-from "resources/day1-input.txt"))
  ;; correct answer: 70357
  )

(defn -main [& args]
  (let [input (input-from "resources/day1-input.txt")]
    (println "part 1:" (solve1 input))
    (println "part 2:" (solve2 input))))
