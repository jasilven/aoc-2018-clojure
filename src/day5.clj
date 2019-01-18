(ns day5
  (:require [clojure.string :as str]))

(def input (str/trim (slurp "resources/day5-input.txt")))

(def letters-az "abcdefghijklmnopqrstuvwxyz")
(def delete-rule (into #{} (doall (map #(str % (str/capitalize %)) letters-az))))
(def delete? (into delete-rule (doall (map #(str (last %) (first %)) delete-rule))))

(defn solve1 [input]
  (loop [in (flatten (partition 1 input))
         out []]
    (cond
      (empty? in) (count (filter some? out))
      :else (if (delete? (apply str (take 2 in)))
              (recur (concat (vector (last out)) (drop 2 in) )
                     (drop-last out))
              (recur (drop 1 in)
                     (concat out (vector (first in))))))))

(defn solve2 [input]
  (-> (for [c letters-az
            :let [in (str/replace input (re-pattern (str "(?i)" c)) "")]]
        (solve1 in))
      sort
      first))

(comment
  (solve1 input)
  ;; correct answer 11590
  (solve2 input)
  ;; correct answer 4504
  )

(defn -main [& args]
  (println "part 1:" (solve1 input))
  (println "part 2:" (solve2 input)))
