(ns day18
  (:require [clojure.string :as str]))

(defn input->acremap
  "=> {[<x> <y>] <ch>, ...}"
  [fname]
  (let [rows (into [] (str/split-lines (slurp fname)))]
    (->> (for [y (range (count rows))
               x (range (count (nth rows y)))
               :let [ch (nth (nth rows y) x)]]
           (vector [x y] ch))
         (into {}))))

(defn adjacent-acres [[x y] acremap]
  (->> (list (acremap [x (dec y)])
             (acremap [(inc x) (dec y)])
             (acremap [(inc x) y])
             (acremap [(inc x) (inc y)])
             (acremap [x (inc y)])
             (acremap [(dec x) (inc y)])
             (acremap [(dec x) y])
             (acremap [(dec x) (dec y)]))))

(defn tree-lumb-cnts
  "return map containing counts of tree and lumb around acre"
  [acre acremap]
  (let [adjacent (adjacent-acres acre acremap)]
    {:tree (count (filter #{\|} adjacent))
     :lumb (count (filter #{\#} adjacent))}))

(defn transform-acre [acre acremap]
  (let [tl-cnts (tree-lumb-cnts acre acremap)]
    (case (acremap acre)
      \. (if (>= (tl-cnts :tree) 3) \| \.)
      \| (if (>= (tl-cnts :lumb) 3) \# \|)
      \# (if (and (>= (tl-cnts :lumb) 1)
                  (>= (tl-cnts :tree) 1)) \# \.))))

(defn calc-total [acremap]
  (let [values (vals acremap)]
    (* (count (filter #{\#} values))
       (count (filter #{\|} values)))))

(defn transform-acremap [{:keys [result acremap minute] :as all}]
  (loop [result {}
         acres (keys acremap)]
    (if (empty? acres)
      (assoc all
             :acremap result
             :total (calc-total result)
             :minute (inc minute))
      (recur (assoc result
                    (first acres)
                    (transform-acre (first acres) acremap))
             (rest acres)))))

(defn solve1 [fname limit]
  (let [acremap (input->acremap fname)
        after-limit (nth (iterate transform-acremap
                                  {:acremap acremap
                                   :acre-hash {:hash (hash acremap) :minute 0}
                                   :total 0
                                   :minute 0}) limit)]
    (after-limit :total)))

(defn transform-acremap2 [{:keys [acremap minute acre-hash target-minute] :as all}]
  (loop [result {}
         acres (keys acremap)]
    (if (empty? acres)
      (let [h (hash result)
            minute (inc minute)]
        (if-let [seen1 (acre-hash h)]
          (assoc all :result (+ seen1 (mod target-minute (- minute seen1))))
          (assoc all
                 :acre-hash (assoc acre-hash (hash result) minute)
                 :acremap result
                 :minute minute)))
      (recur (assoc result
                    (first acres)
                    (transform-acre (first acres) acremap))
             (rest acres)))))

(defn solve2 [fname limit]
  (let [acremap (input->acremap fname)
        result (first (drop-while #(nil? (% :result))
                                  (iterate transform-acremap2
                                           {:acremap acremap
                                            :acre-hash {(hash acremap) 0}
                                            :target-minute limit
                                            :minute 0})))]
    (solve1 fname (result :result))))

(comment
  (solve1 "resources/day18-input.txt" 10)
  ;; correct answer 675100
  (solve2 "resources/day18-input.txt" 1000000000)
  ;; correct answer 191820
  )

(defn -main []
  (println "part 1:" (solve1 "resources/day18-input.txt" 10))
  (println "part 2:" (solve2 "resources/day18-input.txt" 1000000000)))
