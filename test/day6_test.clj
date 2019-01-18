(ns day6-test
  (:require [day6 :refer :all]
            [clojure.test :refer :all]))

(deftest day6-test
  (testing "part 1"
    (is (= 17 (solve1 (input->xys "resources/day6-test-input.txt")))))
  (testing "part 2"
    (is (= 16 (solve2 (input->xys "resources/day6-test-input.txt") 32)))))
