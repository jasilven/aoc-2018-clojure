(ns day2-test
  (:require [day2 :refer :all]
            [clojure.test :refer :all]))

(deftest day2-test
  (testing "day2 part1"
    (is (= 12 (solve1 ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"]))))
  (testing "day2 part2"
    (is (= "fgij" (solve2 ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])))))

