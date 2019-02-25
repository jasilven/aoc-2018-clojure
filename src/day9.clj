(ns day9
  (:require [clojure.string :as str]))

(defn list-init [n]
  (let [node (transient {:value n})]
    (assoc! node :next node)
    (assoc! node :prev node)
    node))

(defn list-insert [{:keys [next] :as root} n]
  (let [node (transient {:value n})]
    (assoc! node :next next :prev root)
    (assoc! next :prev node)
    (assoc! root :next node)
    node))

(defn list-remove [{:keys [next prev value] :as node}]
  (assoc! next :prev prev)
  (assoc! prev :next next)
  [next (:value node)])

(defn list-nth [node n]
  (nth (iterate (if (neg? n) :prev :next) node) (Math/abs (int n))))

(defn remove-marble [{:keys [n player cur scores] :as game}]
  (let [[next-cur points] (list-remove (list-nth cur -7))
        score (+ (get scores player 0) n points)]
    (-> game
        (assoc :cur next-cur)
        (assoc-in [:scores player] score))))

(defn add-marble [{:keys [n cur] :as game}]
  (assoc game :cur (list-insert (:next cur) n)))

(defn play-fn [{:keys [players]}]
  (fn [{:keys [n last] :as game}]
    (-> (if (zero? (mod n 23))
          (remove-marble game)
          (add-marble game))
        (update :n inc)
        (update :player #(mod (inc %) players)))))

(defn solve [players limit]
  (let [play (play-fn {:players players})
        game (iterate play {:n      1
                            :player 1
                            :cur    (list-init 0)
                            :scores {}})]
    (->> (nth game (dec limit))
         (:scores)
         (sort-by (comp - val))
         first
         second)))

(comment
  (solve 468 71843)
  ;; correct answer 385820
  (solve 468 (* 100 71843))
  ;; correct answer 3156297594
  )

(defn -main [& args]
  (println "part 1:" (solve 468 71843))
  (println "part 2:" (solve 468 (* 100 71843))))
