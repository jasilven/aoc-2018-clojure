(ns day14
  (:require [clojure.string :as str]))

(defn gen-scores
  "return new scores => [<int> ...]"
  [elves scores]
  (let [sum (reduce + (map #(nth scores %) elves))]
    (->> (flatten (str/split (str sum) #""))
         (mapv #(Integer/parseInt %)))))

(defn update-elves
  "return new currents for each elf => [<int> ...]"
  [[e1 e2] scores cnt]
  (vector (mod (+ e1 (inc (nth scores e1))) cnt)
          (mod (+ e2 (inc (nth scores e2))) cnt)))

(defn add-scores
  "add new scores to the scoreboard and return new scores"
  [{:keys [scores cnt] :as all} new-scores]
  (loop [ss scores
         i 0]
    (if (= i (count new-scores))
      (assoc all
             :cnt (+ cnt (count new-scores))
             :scores ss)
      (recur (assoc ss (+ i cnt) (new-scores i))
             (inc i)))))

(defn solve1 [elves {:keys [scores limit] :as all}]
  (let [new-scores (gen-scores elves scores)
        new-all (add-scores all new-scores)]
    (if (>= (new-all :cnt) (+ limit 10))
      (apply str (take 10 (drop limit (new-all :scores))))
      (recur (update-elves elves scores (new-all :cnt))
             new-all))))

(defn make-data
  ([limit] (make-data limit "0"))
  ([limit target]
   (let [target (mapv #(Integer/parseInt %) (str/split target #""))]
     {:scores (vec (concat [3 7] (repeat (+ limit 11) 0)))
      :cnt 2
      :limit limit
      :target target})))

(defn result?
  "check if target sequence of recipes found and return count of recipes before it or nil if not found"
  [{:keys [scores target]}]
  (when (> (count scores) (count target))
    (let [cnt (count scores)
          cmp1 (subvec scores (- cnt (count target)))
          cmp2 (subvec scores (dec (- cnt (count target))) (dec cnt))]
      (cond
        (= cmp1 target)
        (- cnt (count target))
        (= cmp2 target)
        (dec (- cnt (count target)))
        :else nil))))

(defn make-data2 [target]
  (let [target (mapv #(Integer/parseInt %) (str/split target #""))]
    {:scores [3 7]
     :target target}) )

(defn solve2 [elves {:keys [scores target] :as all}]
  (let [new-scores (gen-scores elves scores)
        new-all (assoc all :scores (apply conj scores new-scores))]
    (if-let [result (result? new-all)]
      result
      (recur (update-elves elves
                           (new-all :scores)
                           (count (new-all :scores)))
             new-all))))

(comment
  (solve1 [0 1] (make-data 598701))
  ;; correct answer 2776141917
  (solve2 [0 1] (make-data2 "598701"))
  ;; correct answer 20331097
  )

(defn -main [& args]
  (println "part 1:" (solve1 [0 1] (make-data 598701)))
  (println "part 2:" (solve2 [0 1] (make-data2 "598701"))))
