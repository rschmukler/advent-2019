(ns day-8-test
  (:require [day-8 :as sut]
            [clojure.test :as t :refer [deftest testing is]]))

(deftest to-layers-test
  (is (= '[((1 2 3)
            (4 5 6))
           ((7 8 9)
            (0 1 2))]
         (sut/to-layers 3 2 "123456789012"))))

(deftest digit-count-in-layer-test
  (let [layer '((1 2 3)
                (4 5 6))]
    (is (= 1 (sut/digit-count-in-layer 1 layer)))
    (is (= 0 (sut/digit-count-in-layer 0 layer)))))
