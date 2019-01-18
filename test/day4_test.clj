(ns day4-test
  (:require [day4 :refer :all]
            [clojure.test :refer :all]
            [clojure.string :as str]))

(def test-input (slurp "resources/day4-test-input.txt"))

(deftest day4-test
  (testing "part 1"
    (is (= 240 (solve1 test-input))))
  (testing "part 2"
    (is (= 4455 (solve2 test-input)))))
