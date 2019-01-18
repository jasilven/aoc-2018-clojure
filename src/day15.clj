(ns day15
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn file->board
  "parse file to board map => {[<x> <y>] <ch>, ...}"
  [fname]
  (let [rows (into [] (str/split-lines (slurp fname)))]
    (->> (for [y (range (count rows))
               x (range (count (nth rows y)))
               :let [ch (nth (nth rows y) x)]]
           (vector [x y] ch))
         (filter some?)
         (into {}))))

(defn positions
  "return seq of positions of ch in board in reading order"
  [ch board]
  (->> board
       (filter #(= (val %) ch))
       (map key)
       (sort-by (juxt second first))))

(defn range-positions
  "return set of range positions for the current position => #{[x y], ..}. if all is true return all positions else only open ones"
  ([[x y] board]
   (range-positions false [x y] board))
  ([all [x y] board]
   (let [ps (vector [x (dec y)] [(inc x) y] [x (inc y)] [(dec x) y])]
     (->> (if all ps (filter #(= \. (board %)) ps))
          (into #{})))))

(defn my-enemy
  "returns enemy of the unit"
  [pos board]
  (if (= \G (board pos)) \E \G))

(defn enemy-in-range?
  "return weakest (in reading order) enemy position or nil"
  [pos {:keys [board hps]}]
  (let [enemy (my-enemy pos board) ]
    (some->> (range-positions true pos board)
             (map #(vector % (board %) (hps %))) ;=> [pos \unit hp]
             (filter #(= (second %) (my-enemy pos board)))
             (group-by last)
             (sort-by key)
             (first)
             (second)
             (map first)
             (sort-by (juxt second first))
             first)))

(defn distances
  "return reachable positions and their distances => {[x y] dist, ..}"
  [pos board]
  (loop [result {pos 0}
         seen #{pos}
         nexts (list pos)]
    (if (empty? nexts)
      result
      (let [current (first nexts)
            inrange (set/difference (range-positions current board)
                                    seen)]
        (recur (merge result
                      (into {} (map #(vector % (inc (result current))) inrange)))
               (set/union seen inrange)
               (concat (rest nexts) inrange))))))

(defn nearest-target
  "return nearest target position or nil => [x y]"
  [pos board]
  (let [enemy (my-enemy pos board)
        enemy-xys (positions enemy board)
        range-xys (into #{} (mapcat #(range-positions % board) enemy-xys))]
    (some->> (distances pos board)
             (filter #(range-xys (key %)))
             (group-by second)
             (sort-by first)
             (first)
             (second)
             (map first)
             (sort-by (juxt second first))
             first)))

(defn all-units
  "return seq of vectors of all units in reading order => ([[x y] ch] .."
  [board]
  (->> board
       (filter #(case (val %) (\G \E) true false))
       (sort-by #(vector (get-in % [0 1]) (get-in % [0 0])))))

(defn print-game [{:keys [board round hps]}]
  (let [all-xys (keys board)]
    (doseq [y (range (inc (apply max (map second all-xys))))]
      (doseq [x (range (inc (apply max (map first all-xys))))]
        (print (board [x y])))
      (println))))

(defn next-step
  "return next step (position) in reading order => [x y]"
  [from to board]
  (let [distances (distances to board)]
    (->> (for [x (range-positions from board)]
           [x (distances x)])
         (remove #(nil? (second %)))
         (group-by second)
         (sort-by first)
         (first)
         (second)
         (map first)
         (sort-by (juxt second first))
         first)))

(defn attack
  "attack and return new game and enemy pos if attacted => [game [x y]]"
  [pos {:keys [board hps epower] :as game}]
  (if-let [enemy (enemy-in-range? pos game)]
    (do
      [(assoc game :hps (update hps enemy - (if (= \E (board pos)) epower 3))) enemy])
    [game nil]))

(defn move-then-attack
  "move if possible and then attack, always return game"
  [pos {:keys [board hps] :as game}]
  (if-let [nearest (nearest-target pos board)]
    (let [new-pos (next-step pos nearest board)
          new-board (assoc board pos \. new-pos (board pos))
          new-hps (dissoc (assoc hps new-pos (hps pos)) pos)]
      (first (attack new-pos (assoc game :board new-board :hps new-hps))))
    game))

(defn remove-dead-units
  "return vector of two items: set of dead unit positions and game with dead units removed"
  [{:keys [board hps] :as game}]
  (let [dead-units (map first (filter #(<= (val %) 0) hps))
        new-board (if-not (empty? dead-units)
                    (apply assoc board (interleave dead-units (repeat \.)))
                    board)
        new-hps (apply dissoc hps dead-units)]
    (vector (into #{} dead-units)
            (assoc game :board new-board :hps new-hps))))

(defn game-over? [{:keys [board]}]
  (<= (count (distinct (map second (all-units board)))) 1))

(defn play [game-for-round]
  (loop [game (update game-for-round :round inc)
         units (map first (all-units (game :board)))]
    (let [[dead-units g] (remove-dead-units game)
          alive-units (remove #(dead-units %) units)]
      (cond
        (and (not-empty units) (game-over? g))
        (update g :round dec)
        (empty? alive-units)
        g
        :else
        (let [unit (first alive-units)
              [new-game attacked-enemy] (attack unit g)]
          (recur (if attacked-enemy
                   new-game
                   (move-then-attack unit g))
                 (rest alive-units)))))))

(defn total-hitpoints [positions hps]
  (->> (for [pos positions] (hps pos))
       (reduce +)))

(defn outcome [{:keys [board hps round] :as game}]
  (let [units (all-units board)
        winner (if (= \E (second (first units)))
                 "Elves"
                 "Goblins")
        total-hp (reduce + (vals hps))
        result (* round total-hp)]
    (print-game game)
    (printf "\nCombat ends after %s full rounds\n" round)
    (printf "%s win with %s total hit points left\n" winner total-hp)
    (printf "Outcome: %s * %s = %s\n" round total-hp result)
    result))

(defn solve1 [fname]
  (let [board (file->board fname)
        units (map first (all-units board))
        games (rest (iterate play {:board board
                                   :hps (into {} (mapv #(vector % 200) units))
                                   :epower 3
                                   :round 0}))]
    (->> games
         (drop-while #(not (game-over? %)))
         first
         outcome)))

(defn outcome2 [{:keys [board hps round] :as game}]
  (let [units (all-units board)
        total-hp (reduce + (vals hps))]
    {:winner (second (first units))
     :result (* round total-hp)
     :unit-count (count (all-units board))}))

(defn simulate-game [game]
  (let [games (rest (iterate play game))]
    (->> games
         (drop-while #(not (game-over? %)))
         first
         outcome2)))

(defn solve2 [fname]
  (let [board (file->board fname)
        units (map first (all-units board))
        hps (into {} (mapv #(vector % 200) units))
        initial-elves-cnt (count (filter #(= (second %) \E) (all-units board)))]

    ((->> (for [i (range 4 201)]
             (simulate-game {:board board
                             :hps hps
                             :epower i
                             :round 0}))
           (drop-while #(not (and (= (% :winner \E)) (= (% :unit-count) initial-elves-cnt))))
           first) :result)))

(comment
  (solve1 "resources/day15-input.txt")
  ;; correct answer 248848
  (solve2 "resources/day15-input.txt")
  ;; correct answer 64848
  )

(defn -main [& args]
  (println "part 1:" (solve1 "resources/day15-input.txt"))
  (println "part 2:" (solve2 "resources/day15-input.txt")))
