(ns day7-test
  (:require [day7 :refer :all]
            [clojure.test :refer :all]))

(deftest day7-test
  (testing "part 1"
    (is (= "CABDFE" (solve1 (input->instructions "resources/day7-test-input.txt")))))
  (testing "part 2"
    (is (= 15 (solve2 (input->instructions "resources/day7-test-input.txt") 2 0)))))

