(ns day25-test
  (:require [day25 :refer :all]
            [clojure.test :refer :all]))

(deftest day25-test
  (testing "test input 1-4"
    (is (= 2 (solve1 "resources/day25-test-input1.txt")))
    (is (= 4 (solve1 "resources/day25-test-input2.txt")))
    (is (= 3 (solve1 "resources/day25-test-input3.txt")))
    (is (= 8 (solve1 "resources/day25-test-input4.txt")))))
