(ns day-12-test
  (:require [day-12 :as sut]
            [clojure.test :as t :refer [deftest testing is]]))

(deftest pairs-test
  (is (= #{[1 2]
           [2 3]
           [1 3]}
         (set (sut/pairs [1 2 3])))))

(deftest line->pos-test
  (is (= {:x -14 :y 9 :z -4}
         (sut/line->pos "<x=-14, y=9, z=-4>"))))

(deftest apply-velocity-test
  (is (= {:position {:x -1 :y 2 :z 6}
          :velocity {:x -2 :y 0 :z 3}}
         (sut/apply-velocity {:position {:x 1 :y 2 :z 3}
                              :velocity {:x -2 :y 0 :z 3}}))))

(deftest simulate-steps-test
  (let [initial-positions [{:x -1 :y 0 :z 2}
                           {:x 2 :y -10 :z -7}
                           {:x 4 :y -8 :z 8}
                           {:x 3 :y 5 :z -1}]
        moons             (map sut/make-moon initial-positions)]
    (is (= '({:position {:x 2 :y -1 :z 1}
              :velocity {:x 3 :y -1 :z -1}}
             {:position {:x 3 :y -7 :z -4}
              :velocity {:x 1 :y 3 :z 3}}
             {:position {:x 1 :y -7 :z 5}
              :velocity {:x -3 :y 1 :z -3}}
             {:position {:x 2 :y 2 :z 0}
              :velocity {:x -1 :y -3 :z 1}})
           (sut/simulate-steps 1 moons)))))

(deftest kinetic-energy-test
  (is (= 36
         (sut/kinetic-energy {:position {:x 2 :y 1 :z -3}
                              :velocity {:x -3 :y -2 :z 1}}))))
