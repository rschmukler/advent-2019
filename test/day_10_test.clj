(ns day-10-test
  (:require [day-10 :as sut]
            [clojure.test :as t :refer [deftest testing is are]]
            [clojure.string :as str]))

(def sample-input
  (->> [".#..#"
        "....."
        "#####"
        "....#"
        "...##"]
       (str/join "\n")))

(deftest input->asteroids-test
  (is (= #{[1 0]
           [4 0]
           [0 2]
           [1 2]
           [2 2]
           [3 2]
           [4 2]
           [4 3]
           [3 4]
           [4 4]}
         (sut/input->asteroids sample-input))))


(deftest coordinate-distance-test
  (is (= [1 0] (sut/coordinate-distance [0 0] [1 0])))
  (is (= [1 1] (sut/coordinate-distance [2 2] [1 1]))))

(deftest distance-test
  (is (= 1.0 (sut/distance [0 0] [0 1])))
  (is (= 1.0 (sut/distance [0 1] [0 0])))
  (is (= (Math/sqrt 2) (sut/distance [0 0] [1 1]))))

(deftest visible-asteroids-test
  (let [asteroid-set (sut/input->asteroids sample-input)
        result       (into {}
                           (map (juxt identity #(count (sut/visible-asteroids asteroid-set %))))
                           asteroid-set)]
    (is (= {[1 0] 7
            [4 0] 7
            [0 2] 6
            [1 2] 7
            [2 2] 7
            [3 2] 7
            [4 2] 5
            [4 3] 7
            [3 4] 8
            [4 4] 7}
           result))))

(deftest line-projection
  (testing "simple scenario"
    (is (= [1 1] (sut/line-projection [0 0] [1 1]))))


  (testing "reduction"
    (is (= [1 1] (sut/line-projection [0 0] [2 2])))
    (is (= [1 0] (sut/line-projection [0 0] [2 0])))))


(deftest find-best-location-test
  (let [input "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"]
    (is (= [5 8]
           (sut/find-best-location input)))))


(deftest bounding-coordinates-test
  (let [coordinates (set (sut/bounding-coordinates "...\n..."))]
    (is (= #{[0 0]
             [1 0]
             [2 0]
             [0 1]
             [1 1]
             [2 1]}
           coordinates))))

(deftest ->deg-test
  (are [expected line] (is (= expected (sut/->deg line)))
    0.0   [0 -1]
    45.0  [1 -1]
    90.0  [1 0]
    180.0 [0 1]
    270.0 [-1 0]
    315.0 [-1 -1]))


(deftest line->asteroid+vaporize-asteroid-test
  (let [asteroids       #{[-1 0] [1 0] [0 0]}
        line->asteroids (sut/build-line->asteroid asteroids [0 0])]
    (testing "vaproize-asteroid works"
      (let [[a line->asteroids] (sut/vaporize-asteroid line->asteroids [1 0])
            [b line->asteroids] (sut/vaporize-asteroid line->asteroids [1 0])
            [c line->asteroids] (sut/vaporize-asteroid line->asteroids [-1 0])
            d                   (sut/vaporize-asteroid line->asteroids [1 0])]
        (is (= [1 0] a))
        (is (= nil b))
        (is (= [-1 0] c))
        (is (= nil d))))))

(deftest vaporized-asteroids-test
  (let [input  (str/join "\n" [".#..##.###...#######"
                               "##.############..##."
                               ".#.######.########.#"
                               ".###.#######.####.#."
                               "#####.##.#.##.###.##"
                               "..#####..#.#########"
                               "####################"
                               "#.####....###.#.#.##"
                               "##.#################"
                               "#####.##.###..####.."
                               "..######..##.#######"
                               "####.##.####...##..#"
                               ".#####..#.######.###"
                               "##...#.##########..."
                               "#.##########.#######"
                               ".####.#.###.###.#.##"
                               "....##.##.###..#####"
                               ".#.#.###########.###"
                               "#.#.#.#####.####.###"
                               "###.##.####.##.#..##"])
        result (sut/vaporized-asteroids (sut/input->asteroids input) [11 13])]
    (are [idx expected] (= expected (nth result idx))
      0   [11 12]
      1   [12 1]
      2   [12 2]
      199 [8 2]
      298 [11 1])))
