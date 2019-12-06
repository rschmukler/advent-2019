(ns day-6
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn str->edges
  "Transforms an input string into a sequence of edge tuples representing the orbitee
  and orbiter in first and second position respectively"
  [s]
  (->> (str/split-lines s)
       (map #(vec (str/split % #"\)")))))


(defn edges->orbit-map
  ""
  [edges]
  (reduce (fn [m [a b]]
            (-> m
                (update :nodes conj a b)
                (assoc-in [:parents b] a)
                (update-in [:children a] (fnil conj #{}) b)))
          {:nodes    #{}
           :parents  {}
           :children {}}
          edges))

(defn direct-orbit
  "Returns the direct orbit node of `node`"
  [orbit-map node]
  (get-in orbit-map [:parents node]))

(defn parent-orbits
  "Returns a sequence of parent orbits in `orbit-map` for `node`"
  [orbit-map node]
  (->> node
       (iterate #(direct-orbit orbit-map %))
       (drop 1)
       (take-while some?)))

(defn orbit-count
  "Returns the count or orbits (direct or indirect) for the given node"
  [orbit-map node]
  (count (parent-orbits orbit-map node)))

(defn total-orbit-count
  "Returns the total orbit count in the `orbit-map`"
  [orbit-map]
  (reduce + (map #(orbit-count orbit-map %) (:nodes orbit-map))))

(def orbit-map
  (->> (io/resource "input_06")
       (slurp)
       (str->edges)
       (edges->orbit-map)))

(def solve-part-1
  (total-orbit-count orbit-map))

(defn distance-to
  "Returns the distance to the target for the given list of parent orbits. Returns nil
  if we can't get to the target."
  [parent-orbits tgt]
  (loop [orbits parent-orbits
         count  0]
    (when-first [o orbits]
      (if (= tgt o)
        count
        (recur (rest orbits) (inc count))))))

(defn closest-common-ancestor
  "Returns a tuple of the closest common ancestor for `node-a` and `node-b` and the total distance
  to it"
  [orbit-map node-a node-b]
  (let [a-ancestors       (parent-orbits orbit-map node-a)
        b-ancestors       (parent-orbits orbit-map node-b)
        common-ancestors  (set/intersection (set a-ancestors) (set b-ancestors))
        total-distance-to #(+ (distance-to a-ancestors %)
                              (distance-to b-ancestors %))]
    (->> common-ancestors
         (map (juxt identity total-distance-to))
         (sort-by second)
         first)))

(def solve-part-2
  (second (closest-common-ancestor orbit-map "YOU" "SAN")))
