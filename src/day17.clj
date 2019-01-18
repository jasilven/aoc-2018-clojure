(ns day17
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-ints
  "return vector of integers found in string s"
  [s]
  (vec (map #(Integer/parseInt %) (re-seq #"-?\d+" s))))

(defn parse-xy-ranges
  "returns seq of vectors containing x1, x2, y1, y2 ranges parsed from data"
  [data]
  (for [line (str/split-lines data)]
    (let [point (str/split line #", ")
          point (if (= (ffirst point) \x)
                  point
                  [(second point) (first point)])]
      (->> (map #(if (= 1 (count %))
                   [(first %) (first %)]
                   %) (map parse-ints point))
           (flatten)
           vec))))

(defn parse-clay-coords [data]
  (let [xy-ranges (parse-xy-ranges data)]
    (->> (for [[x1 x2 y1 y2] xy-ranges
               x (range x1 (inc x2))
               y (range y1 (inc y2))]
           [x y] )
         (into #{}))))

(defn water-or-clay? [point water clay]
  (or (contains? water point)
      (contains? clay point)))

(defn under-water? [point water clay]
  (let [[x y] point]
    (and (water [x (dec y)])
         (or (or (water [(dec x) (dec y)])
                 (water [(inc x) (dec y)]))
             (and (clay [(dec x) (dec y)])
                  (clay [(inc x) (dec y)]))))))

(defn solo? [[x y] water clay]
  (or (and (clay [(dec x) y])
           (not (water-or-clay? [(inc x) y] water clay)))
      (and (clay [(inc x) y])
           (not (water-or-clay? [(dec x) y] water clay)))))

(defn flow-down [{:keys [point miny maxy clay water edges down corners surface right left] :as all}]
  (let [[x y] point]
    (cond
      (and (empty? edges)
           (or (surface [x (inc y)])
               (= y maxy))) (assoc all :done true)
      (or (= y maxy)
          (and
           (or (some? left) (some? right))
           (or (surface [x (inc y)])
               (surface [(dec x) (inc y)])
               (surface [(inc x) (inc y)])))) (recur (assoc all
                                                            :water water
                                                            :down down
                                                            :point (first edges)
                                                            :edges (rest edges)
                                                            :corners (conj corners (first edges))))
      (clay [x (inc y)]) all
      (water [x (inc y)]) all
      :else (recur (assoc all
                          :point [x (inc y)]
                          :down (if (>= y (dec miny)) (conj down [x (inc y)]) down))))))

(defn flow-right [{:keys [point clay water corners edges right] :as all}]
  (let [[x y] point
        edges (into #{} edges)]
    (loop [x (inc x) water water]
      (cond
        (or (edges [x y]) (corners [x y])) (assoc all :right nil :water water)
        (clay [x y]) (assoc all :right nil :water water)
        (water [x y]) (recur (inc x) water)
        (water-or-clay? [x (inc y)] water clay) (recur (inc x) (conj water [x y]))
        :else (assoc all :right [x y] :water (conj water [x y]))))))

(defn flow-left [{:keys [point clay corners water edges left] :as all}]
  (let [[x y] point
        edges (into #{} edges)]
    (loop [x (dec x) water water]
      (cond
        (or (edges [x y]) (corners [x y])) (assoc all :left nil :water water)
        (clay [x y]) (assoc all :left nil :water water)
        (water [x y]) (recur (dec x) water)
        (water-or-clay? [x (inc y)] water clay) (recur (dec x) (conj water [x y]))
        :else (assoc all :left [x y] :water (conj water [x y]))))))

(defn find-adjacent-row [[x y] xys]
  (let [row (sort-by first (filter #(= (second %) y) xys))
        [smaller larger] (split-with #(< (first %) x) row)]
    (concat (reverse (reduce #(if (= 1 (Math/abs (- (Math/abs (first (last %1)))
                                                    (Math/abs (first %2)))))
                                (conj %1 %2) (reduced %1))
                             [[x y]]
                             (reverse smaller)))
            (rest (reduce #(if (= 1 (Math/abs (- (Math/abs (first %2))
                                                 (Math/abs (first (last %1))))))
                             (conj %1 %2) (reduced %1))
                          [[x y]]
                          (rest larger))))))

(defn update-surface [point surface water clay]
  (->> water
       (find-adjacent-row point)
       (into surface)
       (remove #(under-water? % water clay))
       (into #{})))

(defn update-edges [edges water clay surface]
  (->> edges
       (remove #(under-water? % water clay))
       distinct
       (sort-by second <)
       (into '())))

(defn flow-up [{:keys [point water edges surface clay left right] :as all}]
  (let [[x y] point]
    (if (and (nil? left)
             (nil? right))
      (assoc all :point [x (dec y)] :water (conj water [x y]))
      (let [water (conj water [x y])
            edges (apply conj edges (remove nil? (list left right)))]
        (assoc all
               :point [x (dec y)]
               :water water
               :surface (update-surface point surface water clay)
               :edges (update-edges edges water clay surface))))))

(defn skip-if-done [do-fn {:keys [done] :as all}]
  (if done all (do-fn all)))

(defn flow [{:keys [done point clay water edges left right] :as all}]
  (if done
    all
    (some->> all
             (flow-down)
             (skip-if-done flow-right)
             (skip-if-done flow-left)
             (skip-if-done flow-up))))

(defn initial-state [fname]
  (let [clay (parse-clay-coords (slurp fname))]
    {:miny (apply min (map second clay))
     :maxy (apply max (map second clay))
     :done false
     :clay clay
     :edges '()
     :point [500 0]
     :water #{}
     :corners #{}
     :surface #{}
     :down #{}
     }))

(defn part1 [{:keys [down water]}]
  (+ (count (set/difference down water)) (count water)))

(defn part2 [{:keys [water surface clay]}]
  (let [water-no-surface (set/difference water surface)]
    (->> water-no-surface
         (remove #(solo? % water-no-surface clay))
         count)))

(defn solve [fname]
  (let [state (initial-state fname)]
    (first (drop-while #(not (% :done)) (iterate flow state)))))

(comment
  (part1 (solve "resources/day17-input.txt"))
  ;; correct answer 36171
  (part2 (solve "resources/day17-input.txt"))
  ;; correct answer 28204
  )

(defn -main []
  (let [state (solve "resources/day17-input.txt")]
    (println "part 1:" (part1 state))
    (println "part 2:" (part2 state))))
