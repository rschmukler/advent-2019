(ns day-8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (->> (io/resource "input_08")
       (slurp)
       (str/trim)))

(defn to-layers
  "Converts the provided input into a sequence of `x` by `y` vectors."
  [x y input]
  (->> input
       (map (comp read-string str))
       (partition (* x y))
       (map #(partition x %))
       (into [])))

(defn digit-count-in-layer
  "Returns the number of `digit` in the `layer`"
  [digit layer]
  (count (filter #(= digit %) (flatten layer))))

(def solve-part-1
  (let [layer   (->> (to-layers 25 6 input)
                     (sort-by #(digit-count-in-layer 0 %))
                     first)
        count-1 (digit-count-in-layer 1 layer)
        count-2 (digit-count-in-layer 2 layer)]
    (* count-1 count-2)))

(defn compute-pixel-color
  "Computes the color of a pixel given the collection of values"
  [& pixel-vals]
  (first (remove #{2} pixel-vals)))

(defn compose-layers
  "Creates an `x` x `y` sequence of pixels using `input` by computing
  the pixel color for each pixel in all the layers"
  [x y input]
  (let [input            (map (comp read-string str) input)
        pixels-per-layer (* x y)
        layer-count      (/ (count input) pixels-per-layer)
        pixel-offsets    (iterate #(+ % pixels-per-layer) 0)
        pixel-seqs       (take layer-count (map #(drop % input) pixel-offsets))]
    (partition x (apply map compute-pixel-color pixel-seqs))))

(defn render
  "Renders the given pixels to stdout"
  [pixels]
  (println (apply str (repeat 30 "=")))
  (println)
  (doseq [line pixels]
    (doseq [c line]
      (print (case c
               0 " "
               1 "X")))
    (println))
  (println)
  (println (apply str (repeat 30 "="))))

(def solve-part-2
  (->> input
       (compose-layers 25 6)
       (render)))
