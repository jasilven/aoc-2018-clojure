(ns day19-test
  (:require [day19 :refer :all]
            [clojure.test :refer :all]))

(deftest day19-test
  (testing "part 1"
    (is (= 6 (solve1 "resources/day19-test-input.txt")))))
