(ns day16
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def instructions [:addr :addi :mulr :muli :banr :bani :borr :bori :setr :seti :gtir :gtri :gtrr :eqir :eqri :eqrr])

(defn instruction->index
  "return index of instruction starting from 0"
  [instruction]
  (ffirst (filter #(= (second %) instruction) (partition 2 (interleave (range) instructions)))))

(defn parse-ints
  "return vector of integers found in string s"
  [s]
  (vec (map #(Integer/parseInt %) (re-seq #"-?\d+" s))))

(defn fname->samples&program
  "parse samples and program from file to map"
  [fname]
  (let [[samples program] (str/split (slurp fname) #"\n\n\n\n")
        program (map parse-ints (str/split-lines program))]
    (->> (for [sample (str/split samples #"\n\n")]
           (->> (map parse-ints (str/split-lines sample))
                (interleave [:before :instruction :after])
                (apply assoc {})))
         (assoc {} :program program :samples))))

(defn execute
  "executes instructions using reg as input register and returns modified register"
  [[opnum a b c] reg instructions]
  (case (instructions opnum)
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
    :eqrr (assoc reg c (if (= (reg a) (reg b)) 1 0))))

(defn solve1 [fname]
  (->> (for [sample ((fname->samples&program fname) :samples)]
         (->> (for [instruction instructions]
                (when (= (execute (assoc (sample :instruction) 0 (instruction->index instruction))
                                  (sample :before)
                                  instructions)
                         (sample :after))
                  [((sample :instruction) 0) instruction]))
              (remove nil?)
              count))
       (filter #(> % 2))
       count))

(defn map-instructions-to-opnums
  "find unique instruction/opnums, update result (set) when new instruction/opnum is found"
  [{:keys [samples result] :as input}]
  (assoc input
         :result (->> (for [sample samples]
                        (->> (for [instruction (remove #((apply conj #{} (map second result)) %) instructions)]
                               (when (= (execute (assoc (sample :instruction) 0
                                                        (instruction->index instruction))
                                                 (sample :before)
                                                 instructions)
                                        (sample :after))
                                 [((sample :instruction) 0) instruction]))
                             (remove #(nil? %))))
                      (filter #(= (count %) 1))
                      (map first)
                      (apply conj result))))

(defn execute-instruction [{:keys [instructions program register] :as all}]
  (when (first program)
    (assoc all
           :program (rest program)
           :register (execute (first program) register instructions))))

(defn solve2 [fname]
  (let [input (fname->samples&program fname)
        instructions (into {} (vec ((->> (iterate map-instructions-to-opnums
                                                  {:samples (input :samples)
                                                   :result #{}})
                                         (drop-while #(not= (count (% :result))
                                                            (count instructions)))
                                         first)
                                    :result)))]
    (first ((last (take-while #(not (nil? %))
                              (iterate execute-instruction {:instructions instructions
                                                            :program (input :program)
                                                            :register [0 0 0 0]}))) :register))))

(comment
  (solve1 "resources/day16-input.txt")
  ;; correct answer 607
  (solve2 "resources/day16-input.txt")
  ;; correct answer 577
  )

(defn -main []
  (println "part 1:" (solve1 "resources/day16-input.txt"))
  (println "part 2:" (solve2 "resources/day16-input.txt")))
