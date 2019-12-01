(ns day-1
  (:require [clojure.java.io :as io]))

(defn fuel-required
  "Function to compute the fuel required for a given mass"
  [mass]
  (-> mass
      (/ 3)
      (int)
      (- 2)))


(defn line->mass
  "Function to convert an input line into a mass"
  [line]
  (read-string line))

(def input
  (->>
   (io/resource "input_01")
   (io/reader)
   (line-seq)
   (map line->mass)))

(def part-1-answer
  (->> input
       (map fuel-required)
       (reduce + 0)))


(defn fuel-required-including-fuel-mass
  "Smarter function which takes into account mass added by new fuel
  when computing how much fuel is required"
  [mass]
  (loop [sum  0
         mass mass]
    (let [new-fuel-required (fuel-required mass)]
      (if (pos? new-fuel-required)
        (recur (+ sum new-fuel-required) new-fuel-required)
        sum))))

(def part-2-answer
  (->> input
       (map fuel-required-including-fuel-mass)
       (reduce + 0)))
