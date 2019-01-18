(ns day23-test
  (:require [day23 :refer :all]
            [clojure.test :refer :all]))

(deftest day23-test
  (testing "part 1"
    (is (= 7 (solve1 "resources/day23-test-input.txt")))))
