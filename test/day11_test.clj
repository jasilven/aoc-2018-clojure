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
               (= 61 (result :y))))))
  (testing "part 2"
    (let [result (solve2 18 300 300)]
      (is (and (= 113 (result :power))
               (= 16 (result :size))
               (= 90 (result :x))
               (= 269 (result :y)))))
    (let [result (solve2 42 300 300)]
      (is (and (= 119 (result :power))
               (= 12 (result :size))
               (= 232 (result :x))
               (= 251 (result :y)))))))
