(ns day-2-test
  (:require [day-2 :as sut]
            [clojure.test :as t :refer [is are deftest]]))

(deftest run-test
  (are [in out] (is (= out (sut/run in)))
    [1 0 0 0 99]          [2 0 0 0 99]
    [2 3 0 3 99]          [2 3 0 6 99]
    [2 4 4 5 99 0]        [2 4 4 5 99 9801]
    [1 1 1 4 99 5 6 0 99] [30 1 1 4 2 5 6 0 99]))
