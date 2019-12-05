(ns day-5-test
  (:require [day-5 :as sut]
            [clojure.test :as t :refer [deftest testing is are]]))

(deftest parse-opcode-test
  (is (= [:multiply [:position :immediate :position]] (sut/parse-opcode 1002))))

(deftest get-parameter-test
  (is (= 5 (sut/get-parameter (int-array [1 2 3]) 5 :immediate)))
  (is (= 1 (sut/get-parameter (int-array [1 2 3]) 0 :position))))

(deftest next-instruction-test
  (let [input (int-array [1002 4 3 4 33])]
    (is (= [[:multiply [33 3] [4]]
            4]
           (sut/next-instruction input 0)))))
