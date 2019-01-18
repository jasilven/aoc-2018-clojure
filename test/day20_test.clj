(ns day20-test
  (:require [day20 :refer :all]
            [clojure.test :refer :all]))

(deftest day20-test
  (testing "part 1"
    (is (= 3 (solve1 "WNE")))
    (is (= 10 (solve1 "ENWWW(NEEE|SSE(EE|N))")))
    (is (= 18 (solve1 "ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN")))
    (is (= 23 (solve1 "ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))")))
    (is (= 31 (solve1 "WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))")))))

