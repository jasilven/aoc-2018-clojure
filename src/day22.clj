(ns day22
  (:require [clojure.set :as set])
  (:use clojure.data.priority-map)
  (:refer-clojure :exclude [subseq rsubseq]))


(defn fname->context [fname]
  (let [[d x y] (mapv #(Integer/parseInt %) (re-seq #"\d+" (slurp fname)))]
    {:depth d
     :target [x y]}))

(declare geo-index)

(defn erosion-level [point target depth]
  (mod (+ depth (geo-index point target depth)) 20183))

(def erosion-level-memo (memoize erosion-level))

(defn geo-index [[x y :as point] target depth]
  (cond (#{[0 0] target} point) 0
        (zero? y) (* 16807 x)
        (zero? x) (* 48271 y)
        :else (* (erosion-level-memo [(dec x) y] target depth)
                 (erosion-level-memo [x (dec y)] target depth))))

(defn geo-type [point target depth]
  (if (or (= point target) (= point [0 0])) \.
      (case (mod (erosion-level-memo point target depth) 3)
        0 \.
        1 \=
        2 \|)))

(def geo-type-memo (memoize geo-type))

(defn risk-level [[tx ty :as target] depth]
  (->> (for [y (range 0 (inc ty))
             x (range 0 (inc tx))]
         (case (geo-type [x y] target depth)
           \. 0
           \= 1
           \| 2))
       (reduce +)))

(defn solve1 [{:keys [target depth]}]
  (risk-level target depth))

(defn gears-by-geotype [geotype]
  (case geotype
    \. #{:climb :torch}
    \= #{:climb :neither}
    \| #{:torch :neither}))

(defn update-gear [current xy target depth]
  (let [gt1 (geo-type-memo (:xy current) target depth)
        gt2 (geo-type-memo xy target depth)]
    (if (= gt1 gt2)
      (:gear current)
      (first (set/intersection (gears-by-geotype gt1)
                               (gears-by-geotype gt2))))))

(defn neighbours [target depth {:keys [xy gear] :as current}]
  (let [[x y] xy]
    (->> (vector [x (dec y)] [(inc x) y] [x (inc y)] [(dec x) y])
         (remove #(or (neg? (first %)) (neg? (second %))))
         (map #(hash-map :xy % :gear (update-gear current % target depth))))))

(defn cost [[current minutes] {:keys [gear] :as node}]
  (if (= (:gear current) gear)
    (vector node (inc minutes))
    (vector node (+ 8 minutes))))

(defn update-unvisited [unvisited [node minutes]]
  (if-let [existing (unvisited node)]
    (if (< minutes existing)
      (assoc unvisited node minutes)
      unvisited)
    (assoc unvisited node minutes)))

(defn solve2-dijkstra [target depth]
  (loop [visited {}
         unvisited (priority-map {:xy [0 0] :gear :torch} 0)]
    (let [current (first unvisited)]
      (cond (and (= target (:xy (first current)))
                 (= :torch (:gear (first current)))) (second current)
            (= target (:xy (first current))) (+ 7 (second current))
            :else
            (let [visited (assoc visited (first current) (second current))]
              (recur visited
                     (->> (neighbours target depth (first current))
                          (remove #(contains? visited %))
                          (map #(cost current %))
                          (reduce update-unvisited (dissoc unvisited (first current))))))))))

(defn solve2 [{:keys [target depth]}]
  (solve2-dijkstra target depth))

(comment
  (solve1 (fname->context "resources/day22-input.txt"))
  ;; correct answer 5622
  (solve2 (fname->context "resources/day22-input.txt"))
  ;; correct answer 1089
  )

(defn -main []
  (println "part 1:" (solve1 (fname->context "resources/day22-input.txt")))
  (println "part 2:" (solve2 (fname->context "resources/day22-input.txt"))))
