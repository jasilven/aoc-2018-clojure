(ns day8
  (:require [clojure.string :as str]))

(defn input->numbers [fname]
  (for [s (str/split (str/trim (slurp fname)) #" ")]
    (Integer/parseInt s)))

(defn solve1
  ([input]
   (solve1 input 0 nil nil))
  ([input result child md-count]
   (if (empty? input)
     result
     (cond (nil? child)
           ;; A - root
           (recur (drop 2 input)
                  result
                  (list (first input))
                  (conj md-count (second input)))
           ;; B - middle node, no child
           (and (= 0 (first input)) (> (first child) 1))
           (recur (drop (+ 2 (second input)) input)
                  (+ result (reduce + (take (second input) (drop 2 input))))
                  (conj (rest child) (dec (first child)) )
                  md-count)
           ;; middle node with child
           (and (> (first input) 0) (> (first child) 1))
           (recur (drop 2 input)
                  result
                  (conj (conj (rest child) (dec (first child))) (first input) )
                  (conj md-count (second input)))
           ;; C - last node with child
           (and (> (first input) 0) (= (first child) 1))
           (recur (drop 2 input)
                  result
                  (conj (rest child) (first input))
                  (conj (rest md-count) (+ (first md-count) (second input))))
           ;; last node, no child
           :else
           (recur (drop (+ 2 (second input) (first md-count)) input)
                  (+ result (reduce + (take (+ (second input) (first md-count)) (drop 2 input))))
                  (rest child)
                  (rest md-count))))))

(defn update-result [result parent-meta meta-counts]
  (if (empty? meta-counts)
    result
    (let [childs (into [] (reverse (take-while number? result)))
          new-result (rest (drop-while number? result))]
      (recur (conj new-result
                   (->> (for [n (take (first meta-counts) parent-meta)]
                          (when (and (> n 0) (<= n (count childs)))
                            (nth childs (dec n))))
                        (filter some?)
                        (reduce +)))
             (drop (first meta-counts) parent-meta)
             (rest meta-counts)))))

(defn solve2
  ([input]
   (solve2 input 0 nil nil))
  ([input result child md-count]
   (if (empty? input)
     (first result)
     (cond (nil? child)
           ;; A - root
           (recur (drop 2 input)
                  (list nil)
                  (list (first input))
                  (list (list (second input))))
           ;; B - middle node, no child
           (and (= 0 (first input)) (> (first child) 1))
           (recur (drop (+ 2 (second input)) input)
                  (conj result (reduce + (take (second input) (drop 2 input))))
                  (conj (rest child) (dec (first child)) )
                  md-count)
           ;; middle node with child
           (and (> (first input) 0) (> (first child) 1))
           (recur (drop 2 input)
                  (conj result nil)
                  (conj (conj (rest child) (dec (first child))) (first input) )
                  (conj md-count (list (second input))))
           ;; C - last node with child
           (and (> (first input) 0) (= (first child) 1))
           (recur (drop 2 input)
                  (conj result nil)
                  (conj (rest child) (first input))
                  (conj (rest md-count) (conj (first md-count) (second input))))
           ;; last node, no child
           :else
           (recur (drop (+ 2 (second input) (reduce + (first md-count))) input)
                  (update-result (conj result (reduce + (take (second input) (drop 2 input))))
                                 (take (reduce + (first md-count))
                                       (drop (+ 2 (second input)) input))
                                 (first md-count))
                  (rest child)
                  (rest md-count))))))

(comment
  (solve1 (input->numbers "resources/day8-input.txt"))
  ;; correct answer 47647
  (solve2 (input->numbers "resources/day8-input.txt"))
  ;; correct answer 23636
  )

(defn -main [& args]
  (println "part 1:" (solve1 (input->numbers "resources/day8-input.txt")))
  (println "part 2:" (solve2 (input->numbers "resources/day8-input.txt"))))
