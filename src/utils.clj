(ns utils)

(defn abs
  "Returns the absolute value of `x`"
  [x]
  (max x (- x)))

(defn divisible-by?
  "Returns whether x is divisible by y"
  [x y]
  (= 0 (rem x y)))

(defn least-common-multiple
  "Returns the least common multiple for the provided `xs`"
  [a b & xs]
  (if (or (zero? a) (zero? b) (some? (first (filter zero? xs))))
    0
    (let [abs-a (abs a)
          abs-b (abs b)
          low   (min abs-a abs-b)
          delta (max abs-a abs-b)
          lcm   (->> delta
                     (iterate #(+ delta %))
                     (drop-while #(not (divisible-by? % low)))
                     first)]
      (if (seq xs)
        (apply least-common-multiple lcm xs)
        lcm))))
