(ns day-13
  (:require [intcode-computer :as computer]
            [lanterna.screen :as s]
            [clojure.core.async :refer [go-loop >! >!! <! <!! chan go] :as async])
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

(defn render-game!
  "Takes the provided screen and score and turns it into a string that can be
  rendered"
  [{screen-state :screen
    :keys        [score]}
   screen]
  (let [max-x (apply max (map (comp first key) screen-state))
        max-y (apply max (map (comp second key) screen-state))]
    (doall (for [y (range (+ 1 max-y))
                 x (range (+ 1 max-x))]
             (s/put-string screen x y (tile->str (get screen-state [x y] 0)))))
    (s/put-string screen 0 (inc max-y) (str "Score: " score))))

(defn update-game
  "Updates the `game` with the provided `x` `y` and `tile-id`"
  [game x y tile-id]
  (if (= [-1 0] [x y])
    (assoc game :score tile-id)
    (assoc-in game [:screen [x y]] tile-id)))


(defn make-game
  "Creates a go process that will render the provided screen.

  Returns the atom of the game state"
  [out-chan quit-chan]
  (let [game (atom {:screen {}
                    :score  0})]
    (go-loop []
      (swap! game update-game (<! out-chan) (<! out-chan) (<! out-chan))
      (when-not (async/poll! quit-chan)
        (recur)))
    game))

(defn make-cabinet
  "Create a go process that will render the game to a screen and collect input
  and send it"
  [refresh-rate game-atom in-chan quit-chan]
  (let [screen (s/get-screen)]
    (s/start screen)
    (go-loop []
      (<! (async/timeout (/ 1000 refresh-rate)))
      (render-game! @game-atom screen)
      (s/redraw screen)
      (>! in-chan (case (s/get-key screen)
                    :left  -1
                    :right 1
                    nil    0))
      (when-not (async/poll! quit-chan)
        (recur)))))

(defn solve-part-1
  []
  (let [{:keys [out quit]} (computer/make-program intcode-data)
        m-quit             (async/mult quit)
        new-quit           #(let [c (chan 1)]
                              (async/tap m-quit c)
                              c)
        game               (make-game out (new-quit))]
    (<!! (new-quit))
    (count (filter (comp #{2} val) (:screen @game)))))

(defn -main
  "Starts the game and uses stdin for input. Used for part 2"
  []
  (let [freeplay-program      (cons 2 (rest intcode-data))
        {:keys [in out quit]} (computer/make-program freeplay-program)
        m-quit                (async/mult quit)
        new-quit              #(let [c (chan 1)]
                                 (async/tap m-quit c)
                                 c)
        game                  (make-game out (new-quit))]
    (make-cabinet 5 game in (new-quit))
    (make-game out (new-quit))
    (read-line)))


(comment
  (let [c (async/chan 2)
        m (async/mult c)
        a (chan 1)
        b (chan 1)]
    (async/tap m a)
    (async/tap m b)
    (go (println (<! a)))
    (go (println (<! b)))
    (>!! c 5))
  (-main)
  (println "\033c")
  (solve-part-1))

(comment
  (def screen (s/get-screen))
  (s/start screen)
  (s/redraw screen)
  (render-game! game screen)
  (let [screen (s/get-screen)]
    (s/start screen)
    (rende)
    ))
