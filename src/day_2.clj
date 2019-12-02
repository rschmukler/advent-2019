(ns day-2
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn run
  "Runs the input program and returns the output"
  [in]
  (let [data         (int-array in)
        instructions (partition 4 4 nil data)]
    (loop [op           (first instructions)
           instructions (rest instructions)]
      (if-some [result
                (match (vec op)
                  [99 & _] (vec data)
                  [f i1 i2 o] (let [f  (case f
                                         1 +
                                         2 *)
                                    i1 (aget data i1)
                                    i2 (aget data i2)]
                                (aset-int data o (f i1 i2))
                                nil))]
        result
        (recur (first instructions) (rest instructions))))))

(def input
  (-> (io/resource "input_02")
      (slurp)
      (str/split #",")
      (#(into [] (map read-string) %))))

(defn modified-input
  [verb noun]
  (doto (int-array input)
    (aset-int 1 verb)
    (aset-int 2 noun)))

(def answer-part-1
  (first (run (modified-input 12 2))))

(def delta-per-verb-increment
  (let [x  (first (run (modified-input 0 0)))
        y  (first (run (modified-input 1 0)))
        z  (first (run (modified-input 2 0)))
        d1 (- y x)
        d2 (- z y)]
    (assert (= d1 d2) "Delta is not consistent")
    d1))

(def delta-per-noun-increment
  (let [x  (first (run (modified-input 0 0)))
        y  (first (run (modified-input 0 1)))
        z  (first (run (modified-input 0 2)))
        d1 (- y x)
        d2 (- z y)]
    (assert (= d1 d2) "Delta is not consistent")
    d1))

(defn find-noun-verb
  "Returns a tuple of the noun and verb that will produce the desired target"
  [tgt]
  (let [base         (first (run (modified-input 0 0)))
        needed-delta (- tgt base)]
    [(quot needed-delta delta-per-verb-increment)
     (rem needed-delta delta-per-verb-increment)]))

(def answer-part-2
  (let [[noun verb] (find-noun-verb 19690720)]
    (+ (* 100 noun) verb)))
