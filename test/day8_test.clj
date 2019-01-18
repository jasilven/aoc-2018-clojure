(ns day8-test
  (:require [day8 :refer :all]
            [clojure.test :refer :all]))

(deftest day8-test
  (testing "part 1"
    (is (= 138 (solve1 (input->numbers "resources/day8-test-input.txt"))))
    (is (= 138 (solve1 (list 2 3 1 1 0 1 99 2 0 3 10 11 12 1 1 2)))))
  (testing "part 2"
    (is (= 66 (solve2 (input->numbers "resources/day8-test-input.txt"))))))
