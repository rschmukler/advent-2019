(ns day-13
  (:require [intcode-computer :as computer]
            [clojure.core.async :refer [go-loop >! <! <!! chan] :as async])
  (:gen-class))

(def intcode-data
  (computer/load-intcode-data "input_13"))


(def clear-screen-str
  "Special string to clear a terminal"
  "\033c")

(def key->input
  {"\033[D" -1
   "\033[B" 0
   "\033[C" 1})

(def tile->str
  {0 "  "
   1 "██"
   2 "░░"
   3 "▓▓"
   4 "██"})

(defn game->string
  "Takes the provided screen and score and turns it into a string that can be
  rendered"
  [{:keys [screen score]}]
  (let [max-x          (apply max (map (comp first key) screen))
        max-y          (apply max (map (comp second key) screen))
        screen-content (for [y (range (inc max-y))
                             x (range (inc max-x))]
                         (str (tile->str (get screen [x y] 0))
                              (when (= x max-x)
                                "\n")))]
    (apply str "Score: " score "\n" screen-content)))


(defn make-game
  "Creates a go process that will render the provided screen"
  [refresh-rate out-chan quit-chan]
  (let [screen-refresh (chan (async/dropping-buffer 1))]
    (go-loop []
      (<! (async/timeout (/ 1000 refresh-rate)))
      (>! screen-refresh true)
      (recur))
    (go-loop [game {:screen {}
                    :score  0}]
      (when-not (async/poll! quit-chan)
        (let [x       (<!! out-chan)
              y       (<!! out-chan)
              tile-id (<!! out-chan)
              game    (if (= [-1 0] [x y])
                        (assoc game :score tile-id)
                        (assoc-in game [:screen [x y]] tile-id))]
          (when (async/poll! screen-refresh)
            (println clear-screen-str)
            (println (game->string game)))
          (recur game))))))

(defn run-game
  "Runs the arcade game and returns the output of the screen after quit"
  []
  (let [in-chan   (chan 1)
        out-chan  (chan 1)
        quit-chan (chan 1)]
    (computer/make-program intcode-data in-chan out-chan quit-chan)
    (loop [screen {}]
      (let [x       (<!! out-chan)
            y       (<!! out-chan)
            tile-id (<!! out-chan)
            screen  (assoc screen [x y] tile-id)]
        (if (async/poll! quit-chan)
          screen
          (recur screen))))))

(defn solve-part-1
  []
  (->> (run-game)
       (filter (comp #{2} val))
       (count)))

(defn -main
  "Starts the game and uses stdin for input. Used for part 2"
  []
  (let [in-chan          (chan (async/sliding-buffer 1))
        out-chan         (chan 1)
        quit-chan        (chan 1)
        freeplay-program (cons 2 (rest intcode-data))]
    (computer/make-program freeplay-program in-chan out-chan quit-chan)
    (make-game 60 out-chan quit-chan)
    (go-loop []
      (<! (async/timeout 100))
      (>! in-chan (- 1 (rand-int 2)))
      (recur))
    (read-line)))


(comment
  (println "\033c")
  (solve-part-1))
