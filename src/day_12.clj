(ns day-12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [utils]))

(defn line->pos
  "Parses a line into a position"
  [line]
  (let [[x y z] (map read-string (re-seq #"-?\d+" line))]
    {:x x :y y :z z}))

(def initial-positions
  (->> (io/resource "input_12")
       (slurp)
       (str/split-lines)
       (map line->pos)))

(defn make-moon
  "Creates a new moon with the initial starting position"
  [initial-position]
  {:position initial-position
   :velocity {:x 0 :y 0 :z 0}})

(defn apply-gravity
  "Returns a tuple of `a` and `b` with updated velocities, after taking into
  account their interacting forces"
  [a b]
  (reduce (fn [[a b] k]
            (let [a-val         (-> a :position k)
                  b-val         (-> b :position k)
                  [a-mod b-mod] (cond
                                 (= a-val b-val) [identity identity]
                                 (> a-val b-val) [dec inc]
                                 (< a-val b-val) [inc dec])]
              [(update-in a [:velocity k] a-mod)
               (update-in b [:velocity k] b-mod)]))
          [a b]
          [:x :y :z]))

(defn apply-velocity
  "Returns an updated `moon` with its velocity applied to its position"
  [moon]
  (reduce (fn [moon [k v]]
            (update-in moon [:position k] + v))
          moon
          (:velocity moon)))

(defn pairs
  "Returns a sequence of all unique pairs in a collection"
  [coll]
  (when-let [[a & xs] coll]
    (when (seq xs)
      (lazy-cat (map vector (repeat a) xs)
                (pairs xs)))))

(defn step!
  "Takes a volatile collection of moons and mutates it by one step"
  [volatile-moons]
  (doseq [[a b] (pairs volatile-moons)]
    (let [[new-a new-b] (apply-gravity @a @b)]
      (vreset! a new-a)
      (vreset! b new-b)))
  (doseq [moon volatile-moons]
    (vswap! moon apply-velocity)))


(defn simulate-steps
  "Simulates a `system` for `n` steps"
  [n system]
  (let [moons (map volatile! system)]
    (dotimes [_ n]
      (step! moons))
    (map deref moons)))

(defn kinetic-energy
  "Returns the kinetic energy for a moon"
  [moon]
  (let [potential-energy (reduce + (map utils/abs (vals (:position moon))))
        kinetic-energy   (reduce + (map utils/abs (vals (:velocity moon))))]
    (* potential-energy kinetic-energy)))

(defn total-energy-after-n-steps
  "Returns the total energy in the system after `n` steps"
  [n initial-positions]
  (let [moons (->> initial-positions
                   (map make-moon)
                   (simulate-steps n))]
    (reduce + (map kinetic-energy moons))))

(defn steps-for-cycle-on-axis
  "Returns the number of steps for a cycle on the provided axis"
  [axis initial-positions]
  (let [v-moons (->> initial-positions
                     (map (comp volatile! make-moon)))]
    (loop [seen? #{}]
      (let [hash (doall (map (fn [x]
                               ((juxt #(get-in % [:position axis])
                                      #(get-in % [:velocity axis]))
                                @x)) v-moons))]
        (if (seen? hash)
          (count seen?)
          (do (step! v-moons)
              (recur (conj seen? hash))))))))

(defn steps-for-cycle
  "Returns the number of steps for a cycle to occur in the system.

  To do this it computes the number of cycles required (in parallel) for each
  axis and then returns the least common multiple of those numbers"
  [initial-positions]
  (let [x-steps (future (steps-for-cycle-on-axis :x initial-positions))
        y-steps (future (steps-for-cycle-on-axis :y initial-positions))
        z-steps (future (steps-for-cycle-on-axis :z initial-positions))]
    (utils/least-common-multiple @x-steps @y-steps @z-steps)))


(comment
  ;; Solve part 1
  (total-energy-after-n-steps 1000 initial-positions)
  ;; Solve part 2
  (steps-for-cycle initial-positions)
  ;; 543673227860472
  )
