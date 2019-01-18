(ns day1-test
  (:require  [clojure.test :refer :all]
             [day1 :refer :all]))

(deftest day1-test
  (testing "day1 part1"
    (is (= 3 (solve1 '(1 -2 3 1))))
    (is (= 3 (solve1 '(1 1 1))))
    (is (= 0 (solve1 '(1 1 -2))))
    (is (= -6 (solve1 '(-1 -2 -3)))))
  (testing "day1 part2"
    (is (= 2 (solve2 '(1 -2 3 1))))
    (is (= 0 (solve2 '(1 -1))))
    (is (= 10 (solve2 '(3 3 4 -2 -4))))
    (is (= 5 (solve2 '(-6 3 8 5 -6))))
    (is (= 14 (solve2 '(7 7 -2 -7 -4))))))
