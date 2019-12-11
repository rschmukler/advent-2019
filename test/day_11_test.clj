(ns day-11-test
  (:require [day-11 :as sut]
            [clojure.test :as t :refer [deftest testing is are]]))

(deftest move-test
  (are [x y] (= y (sut/move [0 0] x))
    :left  [-1 0]
    :right [1 0]
    :up    [0 -1]
    :down  [0 1]))
