(ns day3-test
  (:require [day3 :refer [input->claims solve1 solve2]]
            [clojure.test :refer :all]))

(def test-input "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2")

(deftest day3-test
  (testing "input parsing"
    (is (= (list (zipmap [:id :x :y :w :h] [1 1 3 4 4])
                 (zipmap [:id :x :y :w :h] [2 3 1 4 4])
                 (zipmap [:id :x :y :w :h] [3 5 5 2 2]))
           (input->claims test-input))))
  (testing "part 1"
    (is (= 4 (solve1 (input->claims test-input)))))
  (testing "part 2"
    (is (= 3 (solve2 (input->claims test-input))))))
