(ns day18-test
  (:require [day18 :refer :all]
            [clojure.test :refer :all]))

(deftest day18-test
  (testing "part1"
    (is (= 1147 (solve1 "resources/day18-test-input.txt" 10)))))
