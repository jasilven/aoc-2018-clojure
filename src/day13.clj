(ns day13
  (:require [clojure.string :as str]))

(defn trackmap->carts
  "=> {<int> {:x :y :direction :lsr}, ...}"
  [trackmap]
  (->> trackmap
       (filter #(re-matches #"[<>\^v]" (str (val %))))
       (map #(hash-map :x (first (key %))
                       :y (second (key %))
                       :direction (val %)
                       :lsr (cycle [\l \s \r])))
       (zipmap (range))))

(defn clean-trackmap
  "clean carts out of the trackmap and return cleaned trackmap"
  [trackmap]
  (->> trackmap
       (map #(if (re-matches #"[<>\^v]" (str (val %)))
               (vector (key %)
                       (cond
                         (= (val %) \<) \-
                         (= (val %) \>) \-
                         (= (val %) \^) \|
                         (= (val %) \v) \|))
               %))
       (into {})))

(defn input->trackmap
  "=> {[<x> <y>] <ch>, ...}"
  [fname]
  (let [rows (into [] (str/split-lines (slurp fname)))]
    (->> (for [y (range (count rows))
               x (range (count (nth rows y)))
               :let [ch (nth (nth rows y) x)]]
           (when (not= ch \space)
             (vector [x y] ch)))
         (filter some?)
         (into {}))))

(defn move-cart
  "adjust cart's coordinates to new position and return cart => {<cart>}"
  [{:keys [x y direction lsr] :as cart} trackmap]
  (let [track (trackmap [x y])
        new-lsr (if (= track \+) (rest lsr) lsr)
        state (str track direction (first lsr))]
    (-> (cond
          (re-matches #"(\|v.)|(\\>.)|(/<.)|(\+>r)|(\+<l)|(\+vs)" state) ;down
          (assoc cart :y (inc (cart :y)) :direction \v)
          (re-matches #"(->.)|(\\v.)|(/\^.)|(\+>s)|(\+\^r)|(\+vl)" state) ;right
          (assoc cart :x (inc (cart :x)) :direction \>)
          (re-matches #"(\|\^.)|(\\<.)|(/>.)|(\+>l)|(\+<r)|(\+\^s)" state) ;up
          (assoc cart :y (dec (cart :y)) :direction \^)
          (re-matches #"(-<.)|(\\\^.)|(/v.)|(\+<s)|(\+\^l)|(\+vr)" state) ;left
          (assoc cart :x (dec (cart :x)) :direction \<)
          :else (throw (Exception. (str "move not found for state, x, y:" state x y))))
        (assoc :lsr new-lsr))))

(defn collision?
  "if collision with some other cart return vector containing the other cart
  with it's id else return nil => [<id> <cart>]"
  [{:keys [x y]} carts]
  (->> carts
       (filter #(and (= ((second %) :x) x)
                     (= ((second %) :y) y)))
       not-empty
       first))

(defn tick [trackmap carts]
  (loop [cart-keys (map key (sort-by (juxt #((second %) :y)
                                           #((second %) :x)) (into [] carts)))
         carts carts]
    (if (empty? cart-keys)
      [carts nil]
      (let [current-id (first cart-keys)
            moved (move-cart (carts current-id) trackmap)
            new-carts (assoc carts current-id moved)]
        (if-let [cart (collision? moved carts)]
          [new-carts [((second cart) :x) ((second cart) :y)]]
          (recur (rest cart-keys) new-carts))))))

(defn solve1 [fname]
  (let [initial-trackmap (input->trackmap fname)
        trackmap (clean-trackmap initial-trackmap)]
    (loop [carts (trackmap->carts initial-trackmap)]
      (let [[new-carts xy] (tick trackmap carts)]
        (if-not (nil? xy)
          (format "%s,%s" (first xy) (second xy))
          (recur new-carts))))))

(defn tick2 [trackmap carts]
  (loop [cart-keys (map key (sort-by (juxt #((second %) :y)
                                           #((second %) :x)) (into [] carts)))
         carts carts]
    (if (empty? cart-keys)
      carts
      (let [current-id (first cart-keys)
            moved (move-cart (carts current-id) trackmap)
            new-carts (assoc carts current-id moved)
            cart (collision? moved carts)]
        (recur (remove #(= % (first cart)) (rest cart-keys))
               (if cart (dissoc new-carts current-id (first cart)) new-carts))))))

(defn solve2 [fname]
  (let [initial-trackmap (input->trackmap fname)
        trackmap (clean-trackmap initial-trackmap)]
    (loop [carts (trackmap->carts initial-trackmap)]
      (let [new-carts (tick2 trackmap carts)]
        (if (= 1 (count new-carts))
          (format "%s,%s" ((second (first new-carts)) :x) ((second (first new-carts)) :y))
          (recur new-carts))))))

(comment
  (solve1 "resources/day13-input.txt")
  ;; correct answer 83,121
  (solve2 "resources/day13-input.txt")
  ;; correct answer 102,144
  )

(defn -main []
  (println "part 1:" (solve1 "resources/day13-input.txt"))
  (println "part 2:" (solve2 "resources/day13-input.txt")))
