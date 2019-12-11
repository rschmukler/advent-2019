(ns day-11
  (:require [intcode-computer :as computer]
            [clojure.core.async :refer [go-loop >! <! <!! chan]]))

(def ->left
  "Map from a direction to the new direction if we turn left"
  {:up    :left
   :left  :down
   :down  :right
   :right :up})

(def ->right
  "Map from a direction to the new direction if we turn left"
  {:up    :right
   :right :down
   :down  :left
   :left  :up})

(def intcode-data
  (computer/load-intcode-data "input_11"))

(defn move
  "Takes a position and a direction and returns a new position
  as a result of moving in that direction one space"
  [position direction]
  (let [[x y] position]
    (case direction
      :up    [x (dec y)]
      :down  [x (inc y)]
      :right [(inc x) y]
      :left  [(dec x) y])))

(defn make-robot
  [input camera-output hull]
  (go-loop [direction :up
            position [0 0]]
    (>! camera-output (get @hull position 0))
    (when-some [paint-color (<! input)]
      (swap! hull assoc position paint-color)
      (let [turn          (case (<! input)
                            0 ->left
                            1 ->right)
            new-direction (turn direction)
            new-position  (move position new-direction)]
        (recur new-direction new-position)))))


(defn solve-part-1
  []
  (let [hull           (atom {})
        camera-chan    (chan 1)
        program-output (chan 1)
        quit-chan      (chan 1)]
    (computer/make-program intcode-data camera-chan program-output quit-chan)
    (make-robot program-output camera-chan hull)
    (<!! quit-chan)
    (count @hull)))

(defn render-hull
  "Renders the provided hull-map"
  [hull]
  (let [xs    (map first (keys hull))
        ys    (map second (keys hull))
        min-x (apply min xs)
        max-x (apply max xs)
        min-y (apply min ys)
        max-y (apply max ys)]
    (doseq [y (range min-y (inc max-y))]
      (doseq [x (range min-x (inc max-x))]
        (let [char (if (zero? (get hull [x y] 0))
                     "░░"
                     "██")]
          (print char)))
      (println))))

(defn solve-part-2
  []
  (let [hull           (atom {[0 0] 1})
        camera-chan    (chan 1)
        program-output (chan 1)
        quit-chan      (chan 1)]
    (computer/make-program intcode-data camera-chan program-output quit-chan)
    (make-robot program-output camera-chan hull)
    (<!! quit-chan)
    (render-hull @hull)
    @hull))

(comment
  (solve-part-1)
  (solve-part-2))
