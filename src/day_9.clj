(ns day-9
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.async :refer [go-loop <! <!! >!! >! chan alt!!]]
            [clojure.core.async :as async]
            [clojure.set :as set]))

(def ->instruction
  "Lookup map for instructions"
  {1  {:name        :add
       :param-count 3
       :input-count 2}
   2  {:name        :multiply
       :param-count 3
       :input-count 2}
   3  {:name        :input
       :param-count 1
       :input-count 0}
   4  {:name        :output
       :param-count 1
       :input-count 1}
   5  {:name             :jump-if-true
       :param-count      2
       :input-count      2
       :modifies-pointer true}
   6  {:name             :jump-if-false
       :param-count      2
       :input-count      2
       :modifies-poitner true}
   7  {:name        :less-than
       :param-count 3
       :input-count 2}
   8  {:name        :equals
       :input-count 2
       :param-count 3}
   9  {:name        :modify-relative-base
       :input-count 1
       :param-count 1}
   99 {:name        :quit
       :param-count 0
       :input-count 0}})

(def ->parameter-mode
  "Lookup map for an int to a parameter mode"
  {0 :position
   1 :immediate
   2 :relative})

(defn- left-zero-pad
  "Zero pads the provided string to the provided `len`"
  [len s]
  (let [needed-chars (- len (count s))
        padding      (apply str (repeat needed-chars "0"))]
    (str padding s)))

(defn parse-opcode
  "Parses an opcode into a tuple with the instruction name and the parameter modes"
  [opcode]
  (let [digits                    (left-zero-pad 5 (str opcode))
        [param-modes instruction] (split-at 3 digits)
        instruction               (->> instruction
                                       (drop-while #{\0}) ;; trim leading zeros
                                       (map str)
                                       (apply str)
                                       (read-string)
                                       (->instruction))
        _                         (when-not instruction
                                    (throw (ex-info "Error finding instruction" {:opcode opcode
                                                                                 :digits digits})))
        param-modes               (->> param-modes
                                       (reverse)
                                       (take (:param-count instruction))
                                       (map (comp ->parameter-mode read-string str))
                                       (into []))]
    [instruction param-modes]))

(defn ->value
  "Function which loads the parameter value from the `computer`."
  [{:keys [memory relative-base]} mode val]
  (case mode
    :immediate val
    :position  (get memory val 0)
    :relative  (get memory (+ val relative-base) 0)))

(defn ->address
  "Returns the parameter address in the computer for the provided `mode` and value"
  [{:keys [relative-base]} mode val]
  (case mode
    :position val
    :relative (+ val relative-base)))

(defn next-instruction
  "Reads the next instruction from the computer and returns a tuple of the instruction
  and the next instruction pointer address."
  [{:keys [pointer memory] :as computer}]
  (let [[{:keys [name
                 param-count
                 input-count]} param-modes] (parse-opcode (get memory pointer))
        [inputs outputs]                    (->> (range (inc pointer) (+ (inc pointer) param-count))
                                                 (map #(get memory % 0))
                                                 (split-at input-count))
        inputs                              (->> inputs
                                                 (map (partial ->value computer) (take input-count param-modes)))
        outputs                             (->> outputs
                                                 (map (partial ->address computer) (drop input-count param-modes)))
        new-ptr                             (+ 1 param-count pointer)]
    [name inputs outputs new-ptr]))

(defn make-program
  "Creates a program interpriter"
  [data in-chan out-chan quit-chan]
  (go-loop [computer {:memory        (into {} (map-indexed vector) data)
                      :pointer       0
                      :relative-base 0}]
    (when computer
      (let [[instruction [a b] [o] new-ptr] (next-instruction computer)
            computer                        (assoc computer :pointer new-ptr)]
        (recur (case instruction
                 :multiply             (assoc-in computer [:memory o] (* a b))
                 :add                  (assoc-in computer [:memory o] (+ a b))
                 :input                (assoc-in computer [:memory o] (<! in-chan))
                 :output               (do (>!! out-chan a)
                                           computer)
                 :jump-if-true         (if-not (zero? a)
                                         (assoc computer :pointer b)
                                         computer)
                 :jump-if-false        (if (zero? a)
                                         (assoc computer :pointer b)
                                         computer)
                 :less-than            (assoc-in computer [:memory o] (if (< a b) 1 0))
                 :equals               (assoc-in computer [:memory o] (if (= a b) 1 0))
                 :modify-relative-base (update computer :relative-base + a)
                 :quit                 (do (when quit-chan (>! quit-chan true))
                                           (when out-chan (async/close! out-chan))
                                           nil)))))))

(def input
  (-> (io/resource "input_09")
      (slurp)
      (str/split #",")
      (#(into [] (map read-string) %))))

(defn chan->seq
  "Converts a channel into a lazy sequence. Should be called outside of a go block"
  [c]
  (when-some [v (<!! c)]
    (lazy-seq (cons v (chan->seq c)))))

(defn run-program
  [data initial-input]
  (let [in-chan   (chan 1)
        out-chan  (chan 1)
        quit-chan (chan 1)]
    (make-program data in-chan out-chan quit-chan)
    (>!! in-chan initial-input)
    (chan->seq out-chan)))

(defn solve-part-1
  []
  (last (run-program input 1)))

(defn solve-part-2
  []
  (last (run-program input 2)))

(comment
  (solve-part-1)
  (solve-part-2))
