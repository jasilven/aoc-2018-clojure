(ns day19
  (:require [clojure.string :as str]))

(defn fname->initial-context [fname]
  (let [lines (str/split-lines (slurp fname))]
    (->> (for [line (rest lines)
               :let [items (into [] (str/split line #" "))]]
           {:instruction (keyword (items 0))
            :a (Integer/parseInt (items 1))
            :b (Integer/parseInt (items 2))
            :c (Integer/parseInt (items 3))})
         (into [])
         (assoc {}
                :ipreg (Integer/parseInt (second (str/split (first lines) #" ")))
                :ip 0
                :reg [0 0 0 0 0 0]
                :program))))

(defn execute
  "executes next instruction"
  [{:keys [ip program reg] :as all}]
  (let [{:keys [instruction a b c]} (nth program ip)]
    (->> (case instruction
          :addr (assoc reg c (+ (reg a) (reg b)))
          :addi (assoc reg c (+ (reg a) b))
          :mulr (assoc reg c (* (reg a) (reg b)))
          :muli (assoc reg c (* (reg a) b))
          :banr (assoc reg c (bit-and (reg a) (reg b)))
          :bani (assoc reg c (bit-and (reg a) b))
          :borr (assoc reg c (bit-or (reg a) (reg b)))
          :bori (assoc reg c (bit-or (reg a) b))
          :setr (assoc reg c (reg a))
          :seti (assoc reg c a)
          :gtir (assoc reg c (if (> a (reg b)) 1 0))
          :gtri (assoc reg c (if (> (reg a) b) 1 0))
          :gtrr (assoc reg c (if (> (reg a) (reg b)) 1 0))
          :eqir (assoc reg c (if (= a (reg b)) 1 0))
          :eqri (assoc reg c (if (= (reg a) b) 1 0))
          :eqrr (assoc reg c (if (= (reg a) (reg b)) 1 0)))
        (assoc all :reg))))

(defn print-pre-execute [{:keys [ipreg ip program reg] :as context}]
  (let  [{:keys [instruction a b c]} (nth program ip)]
    (printf "ip=%s %s %s %s %s %s " ip reg instruction a b c))
  context)

(defn print-post-execute [{:keys [reg] :as context}]
  (println reg)
  context)

(defn increase-ip [{:keys [ip] :as context}]
  (update context :ip inc))

(defn ip->ipreg [{:keys [ipreg ip reg] :as context}]
  (assoc context :reg (assoc reg ipreg ip)))

(defn ipreg->ip [{:keys [ipreg ip reg] :as context}]
  (assoc context :ip (nth reg ipreg)))

(defn run [{:keys [ipreg ip program reg] :as context}]
  (-> context
      (ip->ipreg)
      #_(print-pre-execute)
      (execute)
      #_(print-post-execute)
      (ipreg->ip)
      (increase-ip)))

(defn solve1 [fname]
  (->> (iterate run (fname->initial-context fname))
       (drop-while #(< (% :ip) (count (% :program))))
       first
       :reg
       first))

(defn solve2 [target]
  "calculate the sum of divisors of the target"
  []
  (reduce + target (filter #(zero? (rem target %)) (range 1 (inc (/ target 2))))))

(comment
  (solve1 "resources/day19-input.txt")
  ;; correct answer 2304
  (solve2 10551330)
  ;; correct answer 28137600
  )

(defn -main []
  (println "part 1:" (solve1 "resources/day19-input.txt"))
  (println "part 2:" (solve2 10551330)))
