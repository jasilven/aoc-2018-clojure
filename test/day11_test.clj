(ns day11-test
  (:require [day11 :refer :all]
            [clojure.test :refer :all]))

(deftest day11-test
  (testing "power-level"
    (is (= 4 (power-level 3 5 8)))
    (is (= -5 (power-level 122 79 57)))
    (is (= 0 (power-level 217 196 39)))
    (is (= 4 (power-level 101 153 71))))
  (testing "part 1"
    (let [result (solve1 18 300 300 3)]
      (is (and (= 33 (result :x))
               (= 45 (result :y)))))
    (let [result (solve1 42 300 300 3)]
      (is (and (= 21 (result :x))
               (= 61 (result :y)))))))
