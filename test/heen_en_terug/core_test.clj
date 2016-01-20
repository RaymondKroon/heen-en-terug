(ns heen-en-terug.core-test
  (:require [clojure.test :refer :all]
            [heen-en-terug.core :refer :all]))

(deftest test-1-1
  (let [result (play-game [:a :b] 2 :a
                          {:a [(create-card :clubs :king) (create-card :clubs :2)]
                           :b [(create-card :clubs :3) (create-card :clubs :queen)]}
                          nil)]
    (is (= result {:a 1 :b 1}))))

(deftest test-0-2-trumped
  (let [result (play-game [:a :b] 2 :a
                          {:a [(create-card :clubs :king) (create-card :clubs :2)]
                           :b [(create-card :spades :3) (create-card :diamonds :queen)]}
                          :spades)]
    (is (= result {:b 2}))))

(deftest test-cheating-suit
  (let [result (play-game [:a :b] 2 :a
                          {:a [(create-card :clubs :king) (create-card :clubs :2)]
                           :b [(create-card :diamonds :3) (create-card :clubs :queen)]}
                          nil)]
    (is (= result nil))))

(deftest test-illegal-trump
  (let [result (play-game [:a :b :c] 2 :a
                          {:a [(create-card :clubs :king) (create-card :clubs :2)]
                           :b [(create-card :spades :3) (create-card :clubs :queen)]
                           :c [(create-card :spades :2) (create-card :hearts :10)]}
                          :spades)]
    (is (= result nil))))

(deftest deal-test
  (let [deck (into [] (range 0 52))
        result (deal deck [:a :b :c] 3)]
    (is (= {:a [0 1 2] :b [3 4 5] :c [6 7 8]} result))))
