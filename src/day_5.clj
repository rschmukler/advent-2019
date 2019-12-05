(ns day-5
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.java.io :as io]))


(def ->instruction
  "Lookup map for converting a two digit opcode to an instruction"
  {[0 1] :add
   [0 2] :multiply
   [0 3] :input
   [0 4] :output
   [0 5] :jump-if-true
   [0 6] :jump-if-false
   [0 7] :less-than
   [0 8] :equals
   [9 9] :quit})

(def ->parameter-mode
  "Lookup map for an int to a parameter mode"
  {0 :position
   1 :immediate})

(def ->param-count
  "Map converting operations into the count of parameters"
  {:add           3
   :multiply      3
   :input         1
   :output        1
   :jump-if-true  2
   :jump-if-false 2
   :less-than     3
   :equals        3
   :quit          0})

(def ->input-count
  "Map converting operations into the count of inputs (ie. not the write destination)"
  {:add           2
   :multiply      2
   :input         0
   :output        1
   :jump-if-true  2
   :jump-if-false 2
   :less-than     2
   :equals        2
   :quit          0})


(defn- left-zero-pad
  "Zero pads the provided string to the provided `len`"
  [len s]
  (let [needed-chars (- len (count s))
        padding      (apply str (repeat needed-chars "0"))]
    (str padding s)))

(defn parse-opcode
  "Parses an opcode into a tuple with the instruction name and the parameter modes"
  [opcode]
  (let [digits                    (map (comp read-string str) (left-zero-pad 5 (str opcode)))
        [param-modes instruction] (split-at 3 digits)
        instruction               (->instruction (vec instruction))
        param-modes               (into [] (map ->parameter-mode) (reverse param-modes))]
    [instruction param-modes]))

(defn get-parameter
  "Function which gets the value of a parameter for the given value and mode"
  [inputs val mode]
  (case mode
    :immediate val
    :position  (aget inputs val)))

(defn array-slice
  "Returns an array slice"
  [data start end]
  (into [] (map (partial aget data)) (range start end)))

(defn next-instruction
  "Function which reads the next instruction and returns a tuple of the instruction input
  and the rest of the input"
  [data ptr]
  (let [[op param-modes]             (parse-opcode (aget data ptr))
        end-ptr                      (+ 1 ptr (->param-count op))
        params-raw                   (array-slice data (inc ptr) end-ptr)
        [input-params output-params] (split-at (->input-count op) params-raw)
        output-params                (vec output-params)
        input-params                 (vec (map (partial get-parameter data) input-params param-modes))]
    [[op input-params output-params] end-ptr]))

(defn run
  "Runs the input program and returns the output"
  [in]
  (let [data (int-array in)]
    (loop [ptr 0]
      (let [[instruction new-ptr] (next-instruction data ptr)
            new-ptr
            (match instruction
              [:multiply inputs [out]]          (do (aset data out (apply * inputs))
                                                    new-ptr)
              [:add inputs [out]]               (do (aset data out (apply + inputs))
                                                    new-ptr)
              [:input _ [out]]                  (do (aset data out ((comp read-string read-line)))
                                                    new-ptr)
              [:output [x] _]                   (do (println x)
                                                    new-ptr)
              [:jump-if-true [x dest-ptr] _]    (if-not (zero? x)
                                                  dest-ptr
                                                  new-ptr)
              [:jump-if-false [x dest-ptr] _]   (if (zero? x)
                                                  dest-ptr
                                                  new-ptr)
              [:less-than [a b] [out]]          (do (aset data out (if (< a b)
                                                                     1
                                                                     0))
                                                    new-ptr)
              [:equals [a b] [out]]             (do (aset data out (if (= a b)
                                                                     1
                                                                     0))
                                                    new-ptr)
              [:quit _ _]                       nil)]
        (when new-ptr
          (recur new-ptr))))))

(def input
  (-> (io/resource "input_05")
      (slurp)
      (str/split #",")
      (#(into [] (map read-string) %))))

(comment
  (run input))
