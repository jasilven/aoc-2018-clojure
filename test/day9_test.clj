(ns day9-test
  (:require [day9 :refer :all]
            [clojure.test :refer :all]))

(deftest day9-test
  (testing "part1"
    (is (= 8317 (solve 10 1618)))
    (is (= 146373 (solve 13 7999)))
    (is (= 54718 (solve 21 6111)))
    (is (= 37305 (solve 30 5807)))))
