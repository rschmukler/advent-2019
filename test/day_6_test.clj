(ns day-6-test
  (:require  [clojure.test :as t :refer [deftest testing is]]
             [day-6 :as sut]))

(def sample-input
  "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L")

(def sample-input-2
  "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN")

(def orbit-map
  (-> sample-input
      (sut/str->edges)
      (sut/edges->orbit-map)))

(def orbit-map-2
  (-> sample-input-2
      (sut/str->edges)
      (sut/edges->orbit-map)))

(deftest str->edges-test
  (is (= ["COM" "B"] (first (sut/str->edges sample-input)))))


(deftest edges->orbit-map-test
  (is (= {:nodes    #{"COM" "A" "B"}
          :parents  {"A" "COM"
                     "B" "COM"}
          :children {"COM" #{"B" "A"}}}
         (sut/edges->orbit-map [["COM" "A"]
                                ["COM" "B"]]))))


(deftest direct-orbit-test
  (is (= "C" (sut/direct-orbit orbit-map "D"))))

(deftest orbit-count-test
  (is (= 3 (sut/orbit-count orbit-map "D")))
  (is (= 42 (sut/total-orbit-count orbit-map))))

(deftest closest-common-ancestor-test
  (is (= ["D" 4]
         (sut/closest-common-ancestor orbit-map-2 "YOU" "SAN"))))
