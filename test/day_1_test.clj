(ns day-1-test
  (:require  [clojure.test :as t :refer [deftest is are]]
             [day-1 :as sut]))

(deftest fuel-required-test
  (are [x y] (is (= x (sut/fuel-required y)))
    2     12
    2     14
    654   1969
    33583 100756))

(deftest fuel-required-including-fuel-mass-test
  (are [x y] (is (= x (sut/fuel-required-including-fuel-mass y)))
    2     14
    966   1969
    50346 100756))
