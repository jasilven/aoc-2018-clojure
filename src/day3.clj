(ns day3
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp "resources/day3-input.txt"))

(defn input->claims
  "convert input to list of maps representing claims
  => ({:id 1, :x 551, :y 185, :w 21, :h 10}...)"
  [lines]
  (for [line (str/split-lines lines)]
    (let [matcher (re-matcher #"\d+" line)]
      (->> (repeatedly 5 #(re-find matcher))
           (map #(Integer/parseInt %))
           (zipmap [:id :x :y :w :h])))))

(defn claim-coords
  "returns list of coords in format \"x:y\" covered by claim => (\"12:34\"...)"
  [claim]
  (let [cx (claim :x)
        cy (claim :y)]
    (for [x (range cx (+ cx (claim :w)))
          y (range cy (+ cy (claim :h)))]
      (format "%s:%s" x y))))

(defn update-fabric
  "update fabric coords count. each coord in fabric holds the number of claims covering it"
  [fabric coords]
  (loop [xys coords
         fab fabric]
    (if (empty? xys)
      fab
      (recur (rest xys)
             (assoc fab
                    (first xys)
                    (inc (fab (first xys) 0)))))))

(defn solve1 [claims]
  (loop [cs claims
         fabric {}]
    (if (empty? cs)
      (count (filterv #(< 1 %) (vals fabric)))
      (recur (rest cs)
             (update-fabric fabric
                            (claim-coords (first cs)))))))

(defn non-overlapping-coords
  "return seq of coords that have only one claim covering it.
  => (\"12:34\" ...)"
  [claims]
  (loop [cs claims
         fabric {}]
    (if (empty? cs)
      (map key (filterv #(= 1 (val %)) fabric))
      (recur (rest cs)
             (update-fabric fabric
                            (claim-coords (first cs)))))))

(defn solve2 [claims]
  (let [noc (into #{} (non-overlapping-coords claims))]
    (loop [cs claims]
      (if (empty? cs)
        nil
        (if (empty? (set/difference (into #{} (claim-coords (first cs)))
                                    noc))
          ((first cs) :id)
          (recur (rest cs)))))))

(comment
  (solve1 (input->claims input))
  ;; correct answer 108961
  (solve2 (input->claims input))
  ;; correct answer 681
  )

(defn -main [& args]
  (println "part 1:" (solve1 (input->claims input)))
  (println "part 2:" (solve2 (input->claims input))))
