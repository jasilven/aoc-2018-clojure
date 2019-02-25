(ns day14-test
  (:require [day14 :refer :all]
            [clojure.test :refer :all]))

(deftest day14-test
  (testing "part 1"
    (is (= "5158916779" (solve1 [0 1] (make-data 9))))
    (is (= "5941429882" (solve1 [0 1] (make-data 2018)))))
  (testing "part 2"
    (is (= 9 (solve2 [0 1] (make-data2 "51589"))))
    (is (= 18 (solve2 [0 1] (make-data2 "92510"))))
    (is (= 2018 (solve2 [0 1] (make-data2 "59414"))))
    (is (= 5 (solve2 [0 1] (make-data2 "01245"))))))
