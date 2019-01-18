(ns day6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn input->xys
  "convers input to seq of xy xys => ([1 1] [1 6] ...)"
  [fname]
  (->> (for [line (str/split-lines (slurp fname))]
         (str/split line #", "))
       (map #(vector (Integer/parseInt (first %)) (Integer/parseInt (second %))))))

(defn manhattan-dist
  "return manhattan distance between two points"
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defn focusarea-limits
  "return the x and y limits (min and max) of the focus area => [1 2 3 4]"
  [xys]
  [(apply min (map first xys))
   (apply max (map first xys))
   (apply min (map second xys))
   (apply max (map second xys))])

(defn closest-xy
  "return closest xy to x,y or nil if there is multiple with same distance => [1 2]"
  [xys x y]
  (let [dists (sort-by first < (for [xy xys]
                                 [(manhattan-dist [x y] xy) xy]))]
    (when-not (= (ffirst dists)
                 (first (second dists)))
      (second (first dists)))))

(defn calculate-area
  "return seq (representin 2d area) containing closest xy for that position => ([1 2] ...)"
  [xys [x1 x2 y1 y2]]
  (for [y (range y1 (inc y2))
        x (range x1 (inc x2))]
    (closest-xy xys x y)))

(defn calculate-area2
  "return sums of distances from each xys to each point in area => (34 ...)"
  [xys [x1 x2 y1 y2]]
  (for [y (range y1 (inc y2))
        x (range x1 (inc x2))]
    (reduce + (for [xy xys] (manhattan-dist xy [x y])))))

(defn find-infinits
  "return xys that have infite area"
  [area [x1 x2 y1 y2]]
  (let [x (inc (- x2 x1))
        y (inc (- y2 y1))
        down (for [n (range 0 x)] (nth area n))
        up (for [n (range (- (* y x) x) (* y x))] (nth area n))
        left (for [n (range 0 (* y x) x)] (nth area n))
        right (for [n (range (dec x) (* y x) x)] (nth area n))]
    (distinct (filter some? (concat down up left right)))))

(defn solve1 [xys]
  (let [limits (focusarea-limits xys)
        area (calculate-area xys limits)
        infinits (into #{} (find-infinits area limits))
        finits (set/difference (into #{} xys) infinits)]
    (->> area
         (filter #(finits %))
         frequencies
         (sort-by second >)
         first
         second)))

(defn solve2 [xys limit]
  (->> (focusarea-limits xys)
       (calculate-area2 xys)
       (filter #(< % limit))
       count))

(comment
  (solve1 (input->xys "resources/day6-input.txt"))
  ;; correct answer 3293
  (solve2 (input->xys "resources/day6-input.txt") 10000)
  ;; correct answer 45176
  )

(defn -main [& args]
  (let [xys (input->xys "resources/day6-input.txt")]
    (println "part 1:" (solve1 xys))
    (println "part 2:" (solve2 xys 10000))))
