(ns day-3-test
  (:require [day-3 :as sut]
            [clojure.test :as t :refer [deftest testing is are]]))

(deftest parse-instruction-test
  (is (= [:right 45] (sut/parse-instruction "R45"))))

(deftest coordinates-for-instruction-test
  (is (= '([1 0]
           [2 0]
           [3 0])
         (sut/coordinates-for-instruction {:x 0 :y 0} [:right 3]))))

(deftest find-shortest-distance-test
  (are [in answer] (= answer (sut/find-shortest-distance in))
    "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"               159
    "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7" 135))

(deftest find-lowest-signal-delay-test
  (are [in answer] (= answer (sut/find-lowest-signal-delay in))
    "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"               610
    "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7" 410))
