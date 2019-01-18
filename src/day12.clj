(ns day12
  (:require [clojure.string :as str]))

(defn input [fname]
  (let [lines (str/split-lines (slurp fname))]
    (hash-map :state (str/replace (first lines) #"initial state: " "")
              :rules (->> (for [line (drop 2 lines)]
                            (str/split line #" => "))
                          (map #(vector (first %) (second %)))
                          (into {})))))

(defn apply-rules [state rules]
  (let [state (str "....." state ".....")]
    (->> (for [n (range 2 (- (count state) 2))]
           (let [s (subs state (- n 2) (+ n 3))]
             (rules s)))
         flatten
         (apply str))))

(defn generation-fn [rules]
  (fn [state]
    (apply-rules state rules)))

(defn solve1 [{:keys [state rules]} cnt]
  (let [generations (iterate (generation-fn rules) state)]
    (->> (nth generations cnt)
         (zipmap (iterate inc (* cnt -3)))
         (filter #(= (val %) \#))
         (keys)
         (reduce +))))

(defn solve2
  "after 102 generations the increase of plant count is constant 59 per generation"
  [input]
  (+ (* 59 (- 50000000000 102)) (solve1 input 102)))

(comment
  (solve1 (input "resources/day12-input.txt") 20)
  ;; correct answer 3120
  (solve2 (input "resources/day12-input.txt"))
  ;; correct answer 2950000001598
  )

(defn -main [& args]
  (println "part 1:" (solve1 (input "resources/day12-input.txt") 20))
  (println "part 2:" (solve2 (input "resources/day12-input.txt"))))
