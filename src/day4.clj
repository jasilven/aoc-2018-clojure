(ns day4
  (:require [clojure.string :as str]))

(def input (slurp "resources/day4-input.txt"))

(defn input->seq
  "converts input data to stripped sequence format (guard id, sleep start min, sleep end min ...)
  => (:10 5 25 30 55 :99 40 50 :10 24 29 :99 36 46 :99 45 55)"
  [input]
  (for [line (sort (str/split-lines input))]
    (if-let [guard (first (re-find #"#(\d+)" line))]
      (keyword (str/replace guard #"#" ""))
      (Integer/parseInt (last (re-find #":(\d\d)" line))))))

(defn parse-guards
  "converts input data to map of guards and sleep slots
  => {:99 ((40 50) (36 46) (45 55)), :10 ((30 55) (5 25) (24 29))}"
  [input]
  (loop [gseq (input->seq input)
         result '()]
    (if (empty? gseq)
      (apply (partial merge-with into) result)
      (let [guard (first gseq)
            slots (partition 2 (take-while integer? (rest gseq)))]
        (recur (drop (* 2 (count slots)) (rest gseq))
               (conj result {guard slots}))))))

(defn calc-total-sleeps
  "calculates total sleep times for each guard
  => ({:id :99, :sleep 30} {:id :10, :sleep 50})"
  [guards]
  (for [g guards]
    {:id (key g) :sleep (reduce + (map #(- (second %) (first %)) (val g)))}))

(defn find-sleepiest
  "finds the sleepiest guard with total sleep mins => {:id :10, :sleep 50}"
  [guards]
  (->> guards
       calc-total-sleeps
       (sort-by :sleep >)
       first))

(defn find-sleepy-minute
  "returns vector of top sleep minute and total sleep minutes => [39 11]"
  [slots]
  (->> (map #(range (first %) (second %)) slots)
       flatten
       frequencies
       (sort-by val >)
       first))

(defn solve1 [input]
  (let [guards (parse-guards input)
        sleepiest-id ((find-sleepiest guards) :id)]
    (* (Integer/parseInt (name sleepiest-id))
       (first (find-sleepy-minute (sleepiest-id guards))))))

(defn find-top-minute-sleeper
  "returns list containing id, minute and sleep minutes of the top minute sleeper
   => (123 21 33)"
  [guards]
  (->> (for [guard guards]
         (let [sleepy-minute (find-sleepy-minute (val guard))]
           (list (name (key guard)) (first sleepy-minute) (second sleepy-minute))))
       (filter #(some? (second %)))
       (sort-by last >)
       first))

(defn solve2 [input]
  (let [guards (parse-guards input)
        minute-sleeper (find-top-minute-sleeper guards)]
    (* (Integer/parseInt (first minute-sleeper))
       (second minute-sleeper))))

(comment
  (solve1 input)
  ;; correct answer 21083
  (solve2 input)
  ;; correct answer 53024
  )

(defn -main [& args]
  (println "part 1:" (solve1 input))
  (println "part 2:" (solve2 input)))
