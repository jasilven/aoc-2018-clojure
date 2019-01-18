(ns day15-test
  (:require [day15 :refer :all]
            [clojure.test :refer :all]))

(deftest day15-test
  (testing "part 1"
    (is (= 27730 (solve1 "resources/day15-test-input1.txt")))
    (is (= 36334 (solve1 "resources/day15-test-input2.txt")))
    (is (= 39514 (solve1 "resources/day15-test-input3.txt")))
    (is (= 27755 (solve1 "resources/day15-test-input4.txt")))
    (is (= 28944 (solve1 "resources/day15-test-input5.txt")))
    (is (= 18740 (solve1 "resources/day15-test-input6.txt")))))
