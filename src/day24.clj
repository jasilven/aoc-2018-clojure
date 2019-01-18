(ns day24
  (:require [clojure.string :as str]))

(defn parse-immunities-or-weaks [s option]
  (let [pat (if (= option :weaks)
              (re-pattern "weak to [\\w, ]+[;\\)] ")
              (re-pattern "immune to [\\w, ]+[;\\)] "))
        result (re-find pat s)]
    (if-not (nil? result)
      (->> (drop 2 (str/split result #" "))
           (map #(str/replace % #"[,;\)]" ""))
           (map keyword)
           (into #{}))
      #{})))

(defn parse-groups
  "returns vector of groups and group is represented by map."
  [s army]
  (let [lines (str/split-lines s)]
    (->> (for [group lines
               :let [[u hp d i] (map #(Integer/parseInt %) (re-seq #"\d+" group))]]
           {:army army
            :units u
            :hitpoints hp
            :damage d
            :type (keyword (first (str/split (re-find #"\w+ damage" group) #" ")))
            :weaks (parse-immunities-or-weaks group :weaks)
            :immunities (parse-immunities-or-weaks group :immunities)
            :initiative i})
         (interleave (range 1 (inc (count lines))))
         (partition 2)
         (map #(vector (str army "-" (first %)) (second %)))
         (map #(vector (first %) (assoc (second %) :id (first %))))
         (into {}))))

(defn fname->groups
  "returns map of groups accessed by keys"
  [fname]
  (let [[immune infection] (into [] (str/split (str/trim (slurp fname)) #"\n\n" ))
        immune (str/replace immune #"Immune System:\n" "")
        infection (str/replace infection #"Infection:\n" "")]
    (merge (parse-groups immune "immune")
           (parse-groups infection "infection"))))

(defn effective-power [group]
  (* (:units group) (:damage group)))

(defn damage [attacker enemy]
  (cond
    (contains? (:immunities enemy) (:type attacker)) 0
    (contains? (:weaks enemy) (:type attacker))
    (* 2 (effective-power attacker))
    :else (effective-power attacker)))

(defn vec-compare [v1 v2]
  (cond
    (= 1 (compare v1 v2)) -1
    (= -1 (compare v1 v2)) 1
    :else 0))

(defn valid-enemys [attacker groups]
  (let [targeted-ids (into #{} (map #(:target %) (vals groups)))]
    (->> (vals groups)
         (filter #(and (not= (:army attacker) (:army %))
                       (not (contains? (:immunities %)
                                       (:type attacker)))
                       (not (contains? targeted-ids (:id %))))))))

(defn choose-target [attacker-id groups]
  (let [attacker (get groups attacker-id)]
    (if-let [enemys (valid-enemys attacker groups)]
      (->> enemys
           (sort-by (juxt (partial damage attacker)
                          effective-power
                          :initiative) vec-compare)
           first
           :id
           (assoc attacker :target)
           (assoc groups attacker-id))
      (assoc groups attacker-id (assoc attacker :target nil)))))

(defn selection-order [groups]
  (sort-by (juxt effective-power :initiative) vec-compare (vals groups)))

(defn attack-order [groups]
  (sort-by :initiative > (vals groups)))

(defn target-selection [groups]
  (loop [ids (map :id (selection-order groups))
         result groups]
    (if (empty? ids)
      result
      (recur (rest ids)
             (choose-target (first ids) result)))))

(defn attack [attacker groups]
  (if-let [enemy-id (:target attacker)]
    (let [enemy (get groups enemy-id)]
      (->> (update enemy
                   :units
                   #(- % (min % (quot (damage attacker enemy)
                                      (:hitpoints enemy)))))
           (assoc groups enemy-id)))
    groups))

(defn attack-phase [groups]
  (loop [ids (map :id (attack-order groups))
         result groups]
    (if (empty? ids)
      result
      (recur (rest ids)
             (attack (get result (first ids)) result)))))

(defn clean-groups [groups]
  (->> (for [g (vals groups)
             :when (> (:units g) 0)]
         (vector (:id g) (assoc g :target nil)))
       (into {})))

(def seen-groups (atom #{}))

(defn fight [groups]
  (if (@seen-groups groups)
    (hash-map "tie" {:units 0 :army "tie"})
    (do
      (swap! seen-groups conj groups)
      (->> groups
           (target-selection)
           (attack-phase)
           (clean-groups)))))

(defn combat-continues? [groups]
  (let [gs (vals groups)]
    (= 2 (count (into #{} (map :army gs))))))

(defn solve [groups]
  (->> (drop-while combat-continues? (iterate fight groups))
       first
       vals
       (map #(vector (:units %) (:army %)))
       (reduce #(vector (+ (first %1) (first %2))
                        (second %2))
               [0 "none"])))

(defn solve1 [fname]
  (first (solve (fname->groups fname))))

(defn boost-immune [n groups]
  (->> (for [g (vals groups)]
         (vector (:id g)
                 (if (= (:army g) "immune")
                   (update g :damage + n) g)))
       (into {})))

(defn solve2 [fname]
  (let [groups (fname->groups fname)]
    (reset! seen-groups #{})
    (loop [n 0
           result [0 nil]]
      (if (= "immune" (second result))
        (first result)
        (recur (inc n)
               (solve (boost-immune (inc n) groups)))))))

(comment
  (solve1 "resources/day24-input.txt")
  ;; correct answer 25088
  (solve2 "resources/day24-input.txt")
  ;; correct answer 2002
  )

(defn -main []
  (println "part 1:" (solve1 "resources/day24-input.txt"))
  (println "part 2:" (solve2 "resources/day24-input.txt")))
