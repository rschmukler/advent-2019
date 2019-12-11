(ns day-10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn input->asteroids
  "Converts an input string into a set of coordinates where asteroids are"
  [input]
  (let [lines (str/split-lines input)]
    (into #{}
          (for [[y line] (map-indexed vector lines)
                [x c]    (map-indexed vector line)
                :let     [asteroid-present? (= \#  c)]
                :when    asteroid-present?]
            [x y]))))


(defn coordinate-distance
  "Returns the a tuple of the distance between the x and y coordinates"
  [a b]
  (let [[a-x a-y] a
        [b-x b-y] b
        x-dist    (apply - (reverse (sort [a-x b-x])))
        y-dist    (apply - (reverse (sort [a-y b-y])))]
    [x-dist y-dist]))

(defn distance
  "Returns the distance between two coordinate points"
  [a b]
  (let [[x-dist y-dist] (coordinate-distance a b)]
    (Math/sqrt (+ (* x-dist x-dist)
                  (* y-dist y-dist)))))


(defn divisible-by?
  "Returns whether x is divisible by y"
  [x y]
  (= 0 (rem x y)))

(defn abs
  "Returns the absolute value of `x`"
  [x]
  (max x (- x)))

(defn line-projection
  "Returns a tuple of the smallest discrete x and y that would project
  from `a` to `b`"
  [a b]
  (let [[a-x a-y] a
        [b-x b-y] b
        d-x       (- b-x a-x)
        d-y       (- b-y a-y)
        min-delta (cond
                   (zero? d-x) (abs d-y)
                   (zero? d-y) (abs d-x)
                   :else       (min (abs d-x) (abs d-y)))]
    (reduce (fn [[d-x d-y] factor]
              (if (and (divisible-by? d-x factor)
                       (divisible-by? d-y factor))
                [(/ d-x factor) (/ d-y factor)]
                [d-x d-y]))
            [d-x d-y]
            (reverse (range 2 (inc min-delta))))))


(defn visible-asteroids
  "Takes an `asteroid-set` and `origin` and returns all asteroids
  that are visible from `coords`"
  [asteroid-set origin]
  (->> asteroid-set
       (remove #{origin})
       (sort-by (partial distance origin))
       (reduce (fn [[result projection-seen?] coord]
                 (let [projection (line-projection origin coord)]
                   (if-not (projection-seen? projection)
                     [(conj result coord) (conj projection-seen? projection)]
                     [result projection-seen?])))
               [#{} #{}])
       (first)))


(defn find-best-location
  "Finds the best location for a sensor station using the provided input"
  [asteroids]
  (->> asteroids
       (sort-by #(count (visible-asteroids asteroids %)))
       (last)))

(defn bounding-coordinates
  "Returns a sequence of coorrdinates that will cycle the boundary of the
  input map"
  [input]
  (let [lines      (str/split-lines input)
        line-count (count lines)
        char-count (count (first lines))]
    (for [x     (range char-count)
          y     (range line-count)
          :when (or (= 0 x)
                    (= 0 y)
                    (= (dec line-count) y)
                    (= (dec char-count) x))]
      [x y])))


(defn ->deg
  "Function which takes a line slope and converts it into degrees from the 12 o'clock
  position."
  [[x y]]
  (let [deg (Math/toDegrees (Math/atan2 x (- y)))]
    (if (neg? deg)
      (+ 360 deg)
      deg)))

(defn build-line->asteroid
  "Constructs a map of line slopes to a vector of asteroids that intersect with that line
  sorted by their distance"
  [asteroids origin]
  (reduce (fn [acc asteroid]
            (let [line (line-projection origin asteroid)]
              (update acc line (fnil conj []) asteroid)))
          {}
          (->> asteroids
               (remove #{origin})
               (sort-by (partial distance origin)))))

(defn vaporize-asteroid
  "Removes the next asteroid on the line, returning a tuple of it
  and the new `line->asteroids` collection.

  If `line->asteroids` is empty, returns `nil`"
  [line->asteroids line]
  (when (seq line->asteroids)
    (if-some [[closest-asteroid & xs] (line->asteroids line)]
      [closest-asteroid (if (seq xs)
                          (assoc line->asteroids line xs)
                          (dissoc line->asteroids line))]
      [nil line->asteroids])))

(defn- vaporized-asteroids*
  "Private helper for vaporized asteroids"
  [line->asteroids line-order]
  (let [[line & lines] line-order]
    (when-some [[a new-line->asteroids] (vaporize-asteroid line->asteroids line)]
      (if a
        (lazy-seq (cons a (vaporized-asteroids* new-line->asteroids lines)))
        (vaporized-asteroids* new-line->asteroids (remove #{line} lines))))))

(defn vaporized-asteroids
  "Returns a sequence of asteroids that would be vaporized for the provided
  `asteroids` and laser station coordinate"
  [asteroids origin]
  (let [line->asteroids (build-line->asteroid asteroids origin)
        line-order      (->> line->asteroids
                             (keys)
                             (sort-by ->deg)
                             (cycle))]
    (vaporized-asteroids* line->asteroids line-order)))

(def input
  (-> (io/resource "input_10")
      (slurp)))

(comment
  ;; Solve part 1
  (let [asteroids (input->asteroids input)
        best-loc  (find-best-location asteroids)]
    (count (visible-asteroids asteroids best-loc)))

  ;; Solve part 2
  (let [asteroids (input->asteroids input)
        best-loc  (find-best-location asteroids)
        vap-order (vaporized-asteroids asteroids best-loc)
        [x y]     (nth vap-order 199)]
    (+ y (* 100 x))))
