(ns day-14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn line->reaction
  "Parses a line into a reaction map"
  [line]
  (let [parse-match (fn [[_ qty name]]
                      [name (read-string qty)])
        symbols     (->> (re-seq #"(\d+) ([a-zA-Z]+)" line)
                         (map parse-match))
        input       (into {} (take (dec (count symbols))) symbols)
        [sym qty]   (last symbols)]
    [sym {:qty   qty
          :input input}]))

(defn materials-required
  "Returns a map of the required inputs as well as the excess
  number of `sym` produced"
  [reactions sym qty]
  (let [reaction         (get reactions sym)
        qty-per-reaction (:qty reaction)
        reaction-count   (bigint (Math/ceil (/ qty qty-per-reaction)))
        excess-count     (- (* reaction-count qty-per-reaction) qty)
        inputs           (:input reaction)]
    [(into {} (map (fn [[k v]] [k (* reaction-count v)])) inputs) excess-count]))

(defn ore-required
  "Takes a map of reactions a symbol and a qty and returns how much ore would
  be required"
  ([reactions sym] (ore-required reactions sym 1))
  ([reactions sym qty]
   (let [reactions (assoc reactions "ORE" {:input {"ORE" 1}
                                           :qty   1})]
     (loop [requirements (hash-map sym qty)
            on-hand      {}]
       (let [[requirements
              on-hand]
             (reduce
              (fn [[req on-hand] [sym qty-needed]]
                (let [qty-on-hand       (get on-hand sym 0)
                      qty-used          (min qty-needed qty-on-hand)
                      qty-to-produce    (- qty-needed qty-used)
                      [mats-req excess] (materials-required reactions sym qty-to-produce)]
                  [(merge-with + req mats-req)
                   (-> on-hand
                       (update sym (fnil - 0) qty-used)
                       (update sym (fnil + 0) excess))]))
              [{} on-hand]
              requirements)]
         (if (= 1 (count requirements))
           (get requirements "ORE")
           (recur requirements on-hand)))))))

(defn max-fuel-with-ore
  "Takes a map of reactions, and a qty of starting ore and returns how much
  fuel is produced."
  [reactions ore-count]
  (let [start-at (int (/ ore-count (ore-required reactions "FUEL" 1)))]
    (->> (iterate inc start-at)
         (pmap (juxt identity #(ore-required reactions "FUEL" %)))
         (drop-while (fn [[_ ore-req]] (< ore-req ore-count)))
         (ffirst)
         (dec))))

(def input-reactions
  (->>
   (io/resource "input_14")
   (slurp)
   (str/split-lines)
   (into {} (map line->reaction))))


(comment
  ;; Part 1
  (ore-required input-reactions "FUEL")

  ;; Part 2
  (let [max-ore 1000000000000]
    (max-fuel-with-ore input-reactions max-ore))
  (int (/ 1000000000000 (ore-required input-reactions "FUEL"))))

