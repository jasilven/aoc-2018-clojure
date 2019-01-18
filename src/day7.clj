(ns day7
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn clean-befores [step befores]
  (let [cleaned (remove #(= step %) befores)]
    [step (distinct cleaned)]))

(defn input->instructions
  "convert input to map => {A (C), E (B D F) ...}"
  [fname]
  (->> (for [line (str/split-lines (slurp fname))
             :let [words (str/split line #" ")]]
         [(nth words 1) (nth words 7)])
       (group-by second)
       (map #(vector (key %) (flatten (val %))))
       (map #(clean-befores (first %) (second %)))
       (into {})))

(defn all-steps
  "return set of all steps"
  [instructions]
  (into #{} (concat (keys instructions)
                    (flatten (vals instructions)))))

(defn start-steps
  "return sorted seq of possible start steps"
  [{:keys [steps instructions]}]
  (sort (set/difference steps
                        (into #{} (keys instructions)))))

(defn update-available
  "return seq of available steps for part 1"
  [available result {:keys [steps instructions]} ]
  (let [completed (into #{} result)
        not-seen (set/difference steps completed)]
    (->> (for [step not-seen]
           (when (empty? (set/difference (into #{} (instructions step))
                                         completed))
             step))
         (filter some?)
         (concat available)
         distinct
         sort)))

(defn solve1 [instructions]
  (let [manual {:instructions instructions
                :steps (all-steps instructions)}]
    (loop [available (start-steps manual)
           result []]
      (if (empty? available)
        (apply str result)
        (let [result (conj result (first available))]
          (recur (update-available (rest available) result manual)
                 result))))))

(defn update-available2
  "return seq of available steps for part 2"
  [available assigned result {:keys [steps instructions]}]
  (let [completed (into #{} result)
        not-seen (set/difference steps completed assigned)]
    (->> (for [step not-seen]
           (when (empty? (set/difference (into #{} (instructions step))
                                         completed))
             step))
         (filter some?)
         (concat available)
         distinct
         sort)))

(defn work
  "assign steps for workers, do the work and returns vector containing workers and availables"
  [workers max available secs]
  (let [new-workers (for [n (range 0 (min (count available)
                                          (- max (count workers))))]
                      (vector (nth available n)
                              (+ secs (- (int (first (nth available n))) 64))))
        workers (concat workers new-workers)]
    [(map #(vector (first %) (dec (second %))) workers)
     (drop (count new-workers) available)]))

(defn solve2 [instructions workers secs]
  (let [manual {:instructions instructions
                :steps (all-steps instructions)}
        max-workers workers]
    (loop [available (start-steps manual)
           result []
           count 0
           workers []]
      (if (and (empty? available)
               (empty? workers))
        count
        (let [[workers available] (work workers
                                        max-workers
                                        available
                                        secs)
              completed (map first (filter #(= 0 (second %)) workers))
              result (concat result completed)]
          (recur (update-available2 available
                                    (map first workers)
                                    result
                                    manual)
                 result
                 (inc count)
                 (filter #(> (second %) 0) workers)))))))

(comment
  (solve1 (input->instructions "resources/day7-input.txt"))
  ;; correct answer "CHILFNMORYKGAQXUVBZPSJWDET"
  (solve2 (input->instructions "resources/day7-input.txt") 5 60)
  ;; correct answer 891
  )

(defn -main [& args]
  (println "part 1:" (solve1 (input->instructions "resources/day7-input.txt")))
  (println "part 1:" (solve2 (input->instructions "resources/day7-input.txt") 5 60)))
