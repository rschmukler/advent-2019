(ns day-14-test
  (:require [day-14 :as sut]
            [clojure.test :as t :refer [deftest testing is are]]
            [clojure.string :as str]))


(deftest line->reaction-test
  (are [in out] (= out (sut/line->reaction in))
    "10 ORE => 10 A"              ["A" {:qty   10
                                        :input {"ORE" 10}}]
    "44 AB, 3 BC, 4 CA => 1 FUEL" ["FUEL" {:input {"AB" 44
                                                   "BC" 3
                                                   "CA" 4}
                                           :qty   1}]))


(def sample-input
  (->>
   "9 ORE => 2 A
   8 ORE => 3 B
   7 ORE => 5 C
   3 A, 4 B => 1 AB
   5 B, 7 C => 1 BC
   4 C, 1 A => 1 CA
   2 AB, 3 BC, 4 CA => 1 FUEL"
   (str/split-lines)
   (into {} (map sut/line->reaction))))

(def complex-input
  (->>
   "171 ORE => 8 CNZTR
    7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
    114 ORE => 4 BHXH
    14 VRPVC => 6 BMBT
    6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
    6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
    15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
    13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
    5 BMBT => 4 WPTQ
    189 ORE => 9 KTJDG
    1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
    12 VRPVC, 27 CNZTR => 2 XDBXC
    15 KTJDG, 12 BHXH => 5 XCVML
    3 BHXH, 2 VRPVC => 7 MZWV
    121 ORE => 7 VRPVC
    7 XCVML => 6 RJRHP
    5 BHXH, 4 VRPVC => 5 LTCX"
   (str/split-lines)
   (into {} (map sut/line->reaction))))

(sut/materials-required complex-input "CNZTR" 9)


(deftest ore-required-test
  (testing "simple case"
    (is (= 45 (sut/ore-required sample-input "A" 10)))
    (is (= 64 (sut/ore-required sample-input "B" 24)))
    (is (= 56 (sut/ore-required sample-input "C" 40))))

  (testing "transitive case"
    (is (= 34 (sut/ore-required sample-input "AB")))
    (is (= 165 (sut/ore-required sample-input "FUEL")))
    (is (= 2210736 (sut/ore-required complex-input "FUEL")))))

(deftest max-fuel-with-ore-test
  (is (= 460664 (sut/max-fuel-with-ore complex-input 1000000000000))))
