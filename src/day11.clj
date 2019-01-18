(ns day11)

(defn power-level [x y serial]
  (let [rack (+ 10 x)
        x (* rack (+ serial (* rack y)))
        y (if (> x 99) (Integer/parseInt (str (nth (reverse (str x)) 2))) 0)]
    (- y 5)))

(defn make-grid [w h serial]
  (->> (for [y (range h)
             x (range w)]
         (vector [x y] (power-level (inc x) (inc y) serial)))
       (into {})))

(def make-grid-memo (memoize make-grid))

(declare square-power-memo)

(defn square-power [grid x y size]
  (cond
    (= 1 size)
    (grid [x y])
    (= 0 (mod size 2))
    (+ (square-power-memo grid x y (/ size 2))
       (square-power-memo grid (+ x (/ size 2)) y (/ size 2))
       (square-power-memo grid x (+ y (/ size 2)) (/ size 2))
       (square-power-memo grid (+ x (/ size 2)) (+ y (/ size 2)) (/ size 2)))
    :else
    (+ (square-power-memo grid x y (dec size))
       (reduce + (for [y (range y (+ y size))] (grid [(+ x (dec size)) y])))
       (reduce + (for [x (range x (dec (+ x size)))] (grid [x (+ y (dec size))]))))))

(def square-power-memo (memoize square-power))

(defn solve1 [serial w h square-size]
  (let [grid (make-grid-memo w h serial)]
    (-> (->> (for [y (range (inc (- h square-size)))
                   x (range (inc (- w square-size)))]
               {:x x :y y :power (square-power-memo grid x y square-size)})
             (apply max-key :power))
        (assoc :size square-size)
        (update :x inc)
        (update :y inc))))

(defn solve2 [serial w h]
  (when (= w h)
    (->> (for [n (range 1 w)]
           (solve1 serial w h n))
         (apply max-key :power))))

(comment
  (solve1 7511 300 300 3)
  ;; correct answer 21,22
  ;; {:x 21, :y 22, :power 34, :size 3}
  (solve2 7511 300 300)
  ;; correct answer 235,288,13
  ;; {:x 235, :y 288, :power 147, :size 13}
  (time (solve1 7511 300 300 24))
  )

(defn -main [& args]
  (println "part 1:" (solve1 7511 300 300 3))
  (println "part 2:" (solve2 7511 300 300)))
