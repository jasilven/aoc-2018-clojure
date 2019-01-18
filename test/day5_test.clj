(ns day5-test
  (:require [day5 :refer :all]
            [clojure.test :refer :all]))

(deftest day5-test
  (testing "part 1"
    (is (= 0 (solve1 "aA")))
    (is (= 0 (solve1 "abBA")))
    (is (= 4 (solve1 "abAB")))
    (is (= 6 (solve1 "aabAAB")))
    (is (= 10 (solve1 "dabAcCaCBAcCcaDA"))))
  (testing "part 2"
    (is (= 4 (solve2 "dabAcCaCBAcCcaDA")))))
