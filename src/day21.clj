(ns day21
  (:require [day19]))

(defn solve1 [fname]
  (->> "resources/day21-input.txt"
       day19/fname->initial-context
       (iterate day19/run)
       (drop-while #(not= (% :ip) 28))
       first
       :reg
       second))

(defn not-seen-fn? []
  (let [seen (atom #{})]
    (fn [num]
      (if (@seen num) false
          (swap! seen conj num)))))

(defn solve2 [fname]
  (let [not-seen? (not-seen-fn?)]
    (->> "resources/day21-input.txt"
         day19/fname->initial-context
         (iterate day19/run)
         (filter #(= (% :ip) 28))
         (map #(get-in % [:reg 1]))
         (take-while not-seen?)
         (last))))

(comment
  (solve1 "resources/day21-input.txt")
  ;; correct answer 4682012
  (solve2 "resources/day21-input.txt")
  ;; correct answer 5363733
  )

(defn -main []
  (println "part 1:" (solve1 "resources/day21-input.txt"))
  (println "part 2:" (solve2 "resources/day21-input.txt")))
