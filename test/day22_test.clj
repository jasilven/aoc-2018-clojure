(ns day22-test
  (:require [day22 :refer :all]
            [clojure.test :refer :all]))

(deftest day22-test
  (testing "part 1"
    (is (= 114 (solve1 {:top-left [0 0] :depth 510 :target [10 10]})))))
