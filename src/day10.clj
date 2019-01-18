(ns day10
  (:require [clojure.string :as str]))

(defn input->data [fname]
  (for [line (str/split-lines (slurp fname))]
    (let [matcher (re-matcher #"-?\d+" line)
          nums (repeatedly 4 #(re-find matcher))]
      (into [] (map #(Integer/parseInt %) nums)))))


(defn print-points [{:keys [points prev-limits prev-points secs]}]
  (let [xys (into #{} (map #(format "%s:%s" (first %) (second %)) prev-points))]
    (doseq [y (range (prev-limits 3) (inc (prev-limits 2)))]
      (doseq [x (range (prev-limits 1) (inc (prev-limits 0)))]
        (if (xys (format "%s:%s" x y))
          (print "#")
          (print " ")))
      (println ""))
    (dec secs)))

(defn update-points [points]
  (map #(vector (+ (% 0) (% 2))
                (+ (% 1) (% 3))
                (% 2)
                (% 3)) points))

(defn calc-limits [points]
  (let [xs (map first points)
        ys (map second points)]
    (vector (apply max xs) (apply min xs) (apply max ys) (apply min ys))))

(defn area [limits]
  (when (some? limits)
    (* (- (limits 0) (limits 1))
       (- (limits 2) (limits 3)))))

(defn move-points [{:keys [points secs limits] :as data}]
  (let [ps (update-points points)]
    (assoc data
           :points ps
           :secs (inc secs)
           :prev-limits limits
           :prev-points points
           :limits (calc-limits ps))))

(defn solve [points]
  (->> (rest (iterate move-points (assoc {}
                                         :points points
                                         :secs 0
                                         :prev-limits nil
                                         :prev-points nil
                                         :limits (calc-limits points))))
       (drop-while #(< (area (% :limits)) (area (% :prev-limits))))
       first
       print-points))

(comment
  (solve (input->data "resources/day10-input.txt"))
  ;; correct answer
  ;; part 1:
  ;; ######  #####   ######  #    #  #          ###  ######   ####
  ;; #       #    #       #  #   #   #           #        #  #    #
  ;; #       #    #       #  #  #    #           #        #  #
  ;; #       #    #      #   # #     #           #       #   #
  ;; #####   #####      #    ##      #           #      #    #
  ;; #       #         #     ##      #           #     #     #  ###
  ;; #       #        #      # #     #           #    #      #    #
  ;; #       #       #       #  #    #       #   #   #       #    #
  ;; #       #       #       #   #   #       #   #   #       #   ##
  ;; #       #       ######  #    #  ######   ###    ######   ### #
  ;; part 2: 10867
  )

(defn -main [& args]
  (println "part 1:")
  (let [secs (solve (input->data "resources/day10-input.txt"))]
    (println "part 2:" secs)))
