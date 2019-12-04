(ns day-4
  (:refer-clojure :exclude [==])
  (:require
   [clojure.core.logic :refer :all]
   [clojure.core.logic.fd :as fd]))

(defn exp
  "Utility function to compute the exponent"
  [x n]
  (reduce * (repeat n x)))

(defn num-digits
  "Helper to return the number of digits in `x`"
  [x]
  (-> x Math/log10 int inc))

(defn reverso
  "Goal to ensure that `l` is the reverse of `r`"
  [l r]
  (conde
   [(== l ()) (== r ())]
   [(fresh [la ld ldr]
      (conso la ld l)
      (appendo ldr (list la) r)
      (reverso ld ldr))]))

(defn- product-sumo*
  "Private helper for product sum. Uses `d` as the exponent for the 10s"
  [d sum q]
  (fresh [head tail product rest-sum]
    (conde
     [(emptyo q) (== sum 0)]
     [(conso head tail q)
      (fd/* (exp 10 d) head product)
      (fd/+ product rest-sum sum)
      (product-sumo* (inc d) rest-sum tail)])))


(defn product-sumo
  "Goal to ensure that `q`'s product-sum is `sum`"
  [sum q]
  (fresh [a head]
    (firsto q head)
    (fd/!= head 0)
    (reverso a q)
    (product-sumo* 0 sum a)))

(defn ascending-ordero
  "Goal that confirms that all elements in `q` are in ascending order"
  [q]
  (fresh [head next tail ntail max]
    (conde
     [(emptyo q)]
     [(conso head tail q)
      (conde
       [(emptyo tail)]
       [(conso next ntail tail)
        (fd/>= next head)
        (ascending-ordero tail)])])))

(defn same-adjacento
  "Goal that confirms that `q` contains two items that are adjacent
  and the same"
  [q]
  (fresh [head next ntail tail]
    (conso head tail q)
    (conso next ntail tail)
    (conde
     [(== head next)]
     [(same-adjacento tail)])))

(defn not-membero
  "Goal to ensure that `v` is not contained in `q`"
  [v q]
  (fresh [head tail]
    (conde
     [(emptyo q)]
     [(conso head tail q)
      (fd/!= v head)
      (not-membero v tail)])))

(defn same-adjacent-and-never-seen-againo*
  "Gaol to ensure that `q` contains two items that are adjacent
  and then are never seen again"
  [already-blocked q]
  (fresh [head next ntail tail new-blocked]
    (conso head tail q)
    (conso next ntail tail)
    (conde
     [(== head next)
      (not-membero head ntail)
      (not-membero head already-blocked)]
     [(== head next)
      (conso head already-blocked new-blocked)
      (same-adjacent-and-never-seen-againo* new-blocked tail)]
     [(fd/!= head next)
      (same-adjacent-and-never-seen-againo* already-blocked tail)])))

(defn same-adjacent-and-never-seen-againo
  "Gaol to ensure that `q` contains two items that are adjacent
  and then are never seen again"
  [q]
  (fresh [blocked]
    (emptyo blocked)
    (same-adjacent-and-never-seen-againo* blocked q)))

(defn num-digitso
  "Goal to bind q to the provided `count` of digits"
  [count q]
  (let [digits (repeatedly count lvar)]
    (and*
     [(everyg #(fd/in % (fd/interval 0 9)) digits)
      (== q digits)])))

(defn boundedo
  "Goal to ensure that q's product-sum is bounded
  between `min` and `max`"
  [min max q]
  (fresh [total]
    (conde
     [(num-digitso (num-digits min) q)]
     [(num-digitso (num-digits max) q)])
    (fd/<= total max)
    (fd/>= total min)
    (product-sumo total q)))

(defn solve-part-1
  "Produces the solution for part one"
  [min max]
  (set
   (run* [q]
     (boundedo min max q)
     (ascending-ordero q)
     (same-adjacento q))))

(defn solve-part-2
  "Produces the solution for part one"
  [min max]
  (set
   (run* [q]
     (boundedo min max q)
     (ascending-ordero q)
     (same-adjacent-and-never-seen-againo q))))

(comment
  (count (solve-part-1 138307 654504))
  (count (solve-part-2 138307 654504))
  (count (solve-part-2 112233 112233))
  (count (solve-part-2 123444 123444))
  (run* [q]
    (boundedo 99 122 q)
    (ascending-ordero q)
    (same-adjacent-and-never-seen-againo q)))
