(ns day-9
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.async :refer [go-loop <! <!! >!! >! chan alt!!]]
            [clojure.core.async :as async]))


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
   [0 9] :modify-relative-base
   [9 9] :quit})

(def ->parameter-mode
  "Lookup map for an int to a parameter mode"
  {0 :position
   1 :immediate
   2 :relative})

(def ->param-count
  "Map converting operations into the count of parameters"
  {:add                  3
   :multiply             3
   :input                1
   :output               1
   :jump-if-true         2
   :jump-if-false        2
   :less-than            3
   :equals               3
   :modify-relative-base 1
   :quit                 0})

(def ->input-count
  "Map converting operations into the count of inputs (ie. not the write destination)"
  {:add                  2
   :multiply             2
   :input                0
   :output               1
   :jump-if-true         2
   :jump-if-false        2
   :less-than            2
   :equals               2
   :modify-relative-base 1
   :quit                 0})


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
  [inputs rel-base param-type val mode]
  (case [param-type mode]
    [:input :immediate] val
    [:input :position]  (aget inputs val)
    [:input :relative]  (aget inputs (+ val rel-base))
    [:output :position] val
    [:output :relative] (+ val rel-base)))

(defn array-slice
  "Returns an array slice"
  [data start end]
  (into [] (map (partial aget data)) (range start end)))

(defn next-instruction
  "Function which reads the next instruction and returns a tuple of the instruction input
  and the rest of the input"
  [data ptr rel-base]
  (let [[op param-modes]                       (parse-opcode (aget data ptr))
        end-ptr                                (+ 1 ptr (->param-count op))
        params-raw                             (array-slice data (inc ptr) end-ptr)
        [input-params output-params]           (split-at (->input-count op) params-raw)
        [input-param-modes output-param-modes] (split-at (->input-count op) param-modes)
        input-params                           (vec (map (partial get-parameter data rel-base :input) input-params input-param-modes))
        output-params                          (vec (map (partial get-parameter data rel-base :output) output-params output-param-modes))]
    [[op input-params output-params] end-ptr]))

(defn make-program
  "Creates a program interpriter"
  [data in-chan out-chan quit-chan]
  (let [data (long-array 40960 data)]
    (go-loop [ptr 0
              rel-base 0]
      (let [[instruction new-ptr] (next-instruction data ptr rel-base)
            [new-ptr rel-base]
            (match instruction
              [:multiply inputs [out]]          (do (aset data out (apply * inputs))
                                                    [new-ptr rel-base])
              [:add inputs [out]]               (do (aset data out (apply + inputs))
                                                    [new-ptr rel-base])
              [:input _ [out]]                  (do (aset data out (<! in-chan))
                                                    [new-ptr rel-base])
              [:output [x] _]                   (do (>! out-chan x)
                                                    [new-ptr rel-base])
              [:jump-if-true [x dest-ptr] _]    (if-not (zero? x)
                                                  [dest-ptr rel-base]
                                                  [new-ptr rel-base])
              [:jump-if-false [x dest-ptr] _]   (if (zero? x)
                                                  [dest-ptr rel-base]
                                                  [new-ptr rel-base])
              [:less-than [a b] [out]]          (do (aset data out (if (< a b)
                                                                     1
                                                                     0))
                                                    [new-ptr rel-base])
              [:equals [a b] [out]]             (do (aset data out (if (= a b)
                                                                     1
                                                                     0))
                                                    [new-ptr rel-base])
              [:equals [a b] [out]]             (do (aset data out (if (= a b)
                                                                     1
                                                                     0))
                                                    [new-ptr rel-base])
              [:modify-relative-base [a] []]    [new-ptr (+ rel-base a)]
              [:quit _ _]                       (do
                                                  (when quit-chan
                                                    (>! quit-chan true))
                                                  (when out-chan
                                                    (async/close! out-chan))
                                                  nil))]
        (when new-ptr
          (recur new-ptr rel-base))))))

(def input
  (-> (io/resource "input_09")
      (slurp)
      (str/split #",")
      (#(into [] (map read-string) %))))

(defn chan->vec
  [c]
  (loop [out []]
    (if-some [v (<!! c)]
      (recur (conj out v))
      out)))

(defn run-program
  [data initial-input]
  (let [in-chan   (chan 1)
        out-chan  (chan 1)
        quit-chan (chan 1)]
    (make-program data in-chan out-chan quit-chan)
    (>!! in-chan initial-input)
    (chan->vec out-chan)))

(defn solve-part-1
  []
  (last (run-program input 1)))

(defn solve-part-2
  []
  (last (run-program input 2)))

(comment
  (solve-part-1)
  (solve-part-2))
