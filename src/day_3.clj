(ns day-3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-instruction
  "Parses a single instruction into a tuple with the direction keyword
  and the count."
  [instruction]
  (let [prefix (case (subs instruction 0 1)
                 "U" :up
                 "D" :down
                 "R" :right
                 "L" :left)
        x      (read-string (subs instruction 1))]
    [prefix x]))

(defn str->instructions
  "Converts a string into a sequence of instructions"
  [s]
  (into [] (map parse-instruction) (str/split s #",")))

(def input-str
  (-> (io/resource "input_03")
      (slurp)))


(defn coordinates-for-instruction
  "Returns a sequence of coordinates for the provided instruction, starting from `cursor`"
  [cursor instruction]
  (let [[dir x]          instruction
        tick-args        (case dir
                           :up    [:y inc]
                           :down  [:y dec]
                           :right [:x inc]
                           :left  [:x dec])
        tick             #(apply update % tick-args)
        cursor-positions (->> cursor
                              (iterate tick)
                              (drop 1)
                              (take x))]
    (map (juxt :x :y) cursor-positions)))

(defn wire->coordinate-vec
  "Returns a vector of coordinates that the wire traversed"
  [wire-instructions]
  (reduce (fn [acc wire]
            (let [[x y] (or (last acc) [0 0])]
              (reduce conj acc (coordinates-for-instruction {:x x :y y} wire))))
          []
          wire-instructions))


(defn mh-distance
  "Returns the distance for a given coordinate tuple"
  [[x y]]
  (+ (max x (- x))
     (max y (- y))))

(defn find-closest-intersection-distance
  "Finds the shortest distance for the provided instruction string"
  [input-str]
  (let [wires           (map str->instructions (str/split-lines input-str))
        coordinate-sets (map (comp set wire->coordinate-vec) wires)
        intersections   (apply set/intersection coordinate-sets)
        distances       (->>  intersections
                              (map mh-distance)
                              (sort))]
    (first distances)))


(defn signal-delay
  "Returns the signal delay for the given wire to the provided coordinate"
  [coord wire]
  (let [delay (.indexOf wire coord)]
    (case delay
      -1 nil
      (inc delay))))

(def answer-part-01
  (find-closest-intersection-distance input-str))

(defn find-lowest-signal-delay
  "Finds the lowest signal delay possible for the provided input string"
  [input-str]
  (let [wire-paths    (map (comp wire->coordinate-vec str->instructions) (str/split-lines input-str))
        intersections (apply set/intersection (map set wire-paths))
        signal-delays (map (fn [intersection]
                             (reduce + (map (partial signal-delay intersection) wire-paths)))
                           intersections)]
    (first (sort signal-delays))))

(def answer-part-02)
