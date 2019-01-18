(ns day24-test
  (:require [day24 :refer :all]
            [clojure.test :refer :all]))

(deftest day24-test
  (testing "input file parsing"
    (is (= 4 (count (fname->groups "resources/day24-test-input.txt"))))
    (is (= 17 (:units (get (fname->groups "resources/day24-test-input.txt") "immune-1"))))
    (is (= 5390 (:hitpoints (get (fname->groups "resources/day24-test-input.txt") "immune-1"))))
    (is (= 4507 (:damage (get (fname->groups "resources/day24-test-input.txt") "immune-1"))))
    (is (= 2 (:initiative (get (fname->groups "resources/day24-test-input.txt") "immune-1"))))
    (is (= 2 (count (:weaks (get (fname->groups "resources/day24-test-input.txt") "immune-1")))))
    (is (contains? (:weaks (get (fname->groups "resources/day24-test-input.txt") "immune-1"))
                   :radiation))
    (is (contains? (:weaks (get (fname->groups "resources/day24-test-input.txt") "immune-1"))
                   :bludgeoning))

    (is (= 989 (:units (get (fname->groups "resources/day24-test-input.txt") "immune-2"))))
    (is (= 1274 (:hitpoints (get (fname->groups "resources/day24-test-input.txt") "immune-2"))))
    (is (= 25 (:damage (get (fname->groups "resources/day24-test-input.txt") "immune-2"))))
    (is (= 3 (:initiative (get (fname->groups "resources/day24-test-input.txt") "immune-2"))))
    (is (= 2 (count (:weaks (get (fname->groups "resources/day24-test-input.txt") "immune-2")))))
    (is (contains? (:weaks (get (fname->groups "resources/day24-test-input.txt") "immune-2"))
                   :slashing))
    (is (contains? (:weaks (get (fname->groups "resources/day24-test-input.txt") "immune-2"))
                   :bludgeoning))

    (is (= 801 (:units (get (fname->groups "resources/day24-test-input.txt") "infection-1"))))
    (is (= 4706 (:hitpoints (get (fname->groups "resources/day24-test-input.txt") "infection-1"))))
    (is (= 116 (:damage (get (fname->groups "resources/day24-test-input.txt") "infection-1"))))
    (is (= 1 (:initiative (get (fname->groups "resources/day24-test-input.txt") "infection-1"))))
    (is (= 1 (count (:weaks (get (fname->groups "resources/day24-test-input.txt") "infection-1")))))
    (is (contains? (:weaks (get (fname->groups "resources/day24-test-input.txt") "infection-1"))
                   :radiation))

    (is (= 4485 (:units (get (fname->groups "resources/day24-test-input.txt") "infection-2"))))
    (is (= 2961 (:hitpoints (get (fname->groups "resources/day24-test-input.txt") "infection-2"))))
    (is (= 12 (:damage (get (fname->groups "resources/day24-test-input.txt") "infection-2"))))
    (is (= 4 (:initiative (get (fname->groups "resources/day24-test-input.txt") "infection-2"))))
    (is (= 2 (count (:weaks (get (fname->groups "resources/day24-test-input.txt") "infection-2")))))
    (is (contains? (:weaks (get (fname->groups "resources/day24-test-input.txt") "infection-2"))
                   :fire))
    (is (contains? (:weaks (get (fname->groups "resources/day24-test-input.txt") "infection-2"))
                   :cold))
    (is (contains? (:immunities (get (fname->groups "resources/day24-test-input.txt") "infection-2"))
                   :radiation)))
  (testing "boosting"
    (let [boosted (boost-immune 1570 (fname->groups "resources/day24-test-input.txt"))]
      (is (= 4 (count boosted)))
      (is (= 17 (:units (get boosted "immune-1"))))
      (is (= 5390 (:hitpoints (get boosted "immune-1"))))
      (is (= 6077 (:damage (get boosted "immune-1"))))
      (is (= 2 (:initiative (get boosted "immune-1"))))
      (is (= :fire (:type (get boosted "immune-1"))))
      (is (= 2 (count (:weaks (get boosted "immune-1")))))
      (is (contains? (:weaks (get boosted "immune-1")) :radiation))
      (is (contains? (:weaks (get boosted "immune-1")) :bludgeoning))

      (is (= 989 (:units (get boosted "immune-2"))))
      (is (= 1274 (:hitpoints (get boosted "immune-2"))))
      (is (= 1595 (:damage (get boosted "immune-2"))))
      (is (= :slashing (:type (get boosted "immune-2"))))
      (is (= 3 (:initiative (get boosted "immune-2"))))
      (is (= 2 (count (:weaks (get boosted "immune-2")))))
      (is (contains? (:weaks (get boosted "immune-2")) :slashing))
      (is (contains? (:weaks (get boosted "immune-2")) :bludgeoning))
      (is (contains? (:immunities (get boosted "immune-2")) :fire))

      (is (= 801 (:units (get boosted "infection-1"))))
      (is (= 4706 (:hitpoints (get boosted "infection-1"))))
      (is (= :bludgeoning (:type (get boosted "infection-1"))))
      (is (= 116 (:damage (get boosted "infection-1"))))
      (is (= :bludgeoning (:type (get boosted "infection-1"))))
      (is (= 1 (:initiative (get boosted "infection-1"))))
      (is (= 1 (count (:weaks (get boosted "infection-1")))))
      (is (contains? (:weaks (get boosted "infection-1")) :radiation))

      (is (= 4485 (:units (get boosted "infection-2"))))
      (is (= 2961 (:hitpoints (get boosted "infection-2"))))
      (is (= :slashing (:type (get boosted "infection-2"))))
      (is (= 12 (:damage (get boosted "infection-2"))))
      (is (= 4 (:initiative (get boosted "infection-2"))))
      (is (= 2 (count (:weaks (get boosted "infection-2")))))
      (is (contains? (:weaks (get boosted "infection-2")) :fire))
      (is (contains? (:weaks (get boosted "infection-2")) :fire))
      (is (contains? (:immunities (get boosted "infection-2")) :radiation))))
  (testing "part 1"
    (is (= 5216 (solve1 "resources/day24-test-input.txt"))))
  (testing "part 2"
    (is (= 51 (solve2 "resources/day24-test-input.txt")))))