(ns heen-en-terug.core
  (:require [clojure.core.async :as async])
  (:import [java.security SecureRandom])
  (:gen-class))

(def secure-generator ^SecureRandom (SecureRandom.))

(defn secure-shuffle
  "Return a random permutation of coll"
  [^java.util.Collection coll]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al ^SecureRandom secure-generator )
    (clojure.lang.RT/vector (.toArray al))))

(defrecord Card [suit rank name])

(defmethod print-method Card [v ^java.io.Writer w]
  (.write w (str (.suit v) (.name v))))

(def suits [:spades :clubs :hearts :diamonds])
(def cards {:2 0, :3 1, :4 2, :5 3, :6 4, :7 5, :8 6, :9 7, :10 8
            :jack 9, :queen 10, :king 11, :ace 12})

(defn create-deck []
  (for [suit suits
        [name rank] cards]
    (->Card suit rank name)))

(defn create-card [suit name]
  (->Card suit (name cards) name))

(defn remove-card [deck card]
  (filter (fn [c] (not= card c)) deck))

(defn remove-cards [deck & cards]
  (loop [[card & more-cards] cards
         remaining-deck deck]
    (if card
      (recur more-cards (remove-card remaining-deck card))
      remaining-deck)))

(defn create-shuffler [deck n]
  "returns channel which returns n shuffled decks"
  (let [c (async/chan 10)]
    (async/go-loop [i 0]
      (if (< i n)
        (do (async/>! c (secure-shuffle deck))
            (recur (inc i)))
        (async/close! c)))
    c))

(defn deal [deck players n-cards]
  (loop [[player & more-players] players
         remaining-deck deck
         result {}]
    (if player
      (recur more-players (drop n-cards remaining-deck)
             (assoc result player (into [] (take n-cards remaining-deck))))
      result)))

(defn compare-cards [player-card-a player-card-b trump]
  (let [[player-a card-a] player-card-a
        [player-b card-b] player-card-b]
    (cond
      (= (:suit card-a) (:suit card-b)) (if (> (:rank card-a) (:rank card-b))
                                          player-card-a player-card-b)
      (= (:suit card-b) trump) player-card-b
      :else player-card-a)))

(defn valid-play? [requested-suit trump winning-card played-card remaining-suits]

  (cond
    (and (not (requested-suit (:suit played-card))) (some requested-suit remaining-suits)) false
    (and (not (requested-suit trump))
         (= trump (:suit winning-card))
         (= trump (:suit played-card))
         (> (:rank winning-card) (:rank played-card))) false
    :else true))

(defn play-trick [players trick-i starting-player players-cards trump]
  "return winning player, nil if invalid round"
  (let [order (take (count players) (drop-while #(not= % starting-player) (cycle players)))
        first-player (first order)
        ;_ (print first-player)
        first-card  (-> players-cards (first-player players-cards) (get trick-i))
        requested-suit #{(:suit first-card)}]
    (loop [[player & more-players] (drop 1 order)
           winning-player-card [(first order) first-card]
           valid? true]
      (if (and valid? player)
        (let [player-cards (player players-cards)
              played-card (get player-cards trick-i)
              nw-player-card (compare-cards winning-player-card [player played-card] trump)
              nw-valid? (and valid?
                             (valid-play? requested-suit trump (second winning-player-card) played-card
                                          (map :suit (drop 1 player-cards))))]
          (recur more-players nw-player-card nw-valid?)
          )
        (when valid?
          winning-player-card)))
    ))

(defn play-game [players n-cards first-player player-cards trump]
  "returns won tricks or nil if invalid"
  (loop [trick-i 0
         tricks-won {}
         starting-player first-player]
    (if (< trick-i n-cards)
      (if-let [[player card] (play-trick players trick-i starting-player player-cards trump)]
        (recur (inc trick-i) (update-in tricks-won [player] (fnil inc 0)) player)
        nil)
      tricks-won))
  )

(defn create-table [stats n-ok shuffler play-cards all-players n-cards first-player trump]
  (async/go-loop [deck (async/<! shuffler)]
    (let [player-cards (merge {:a play-cards}
                              (deal deck (rest all-players) n-cards))
              game-result (play-game all-players n-cards :a player-cards trump)]
          (when game-result
            (swap! stats #(merge-with + % {(get game-result :a 0) 1}))
            (swap! n-ok inc)))
    (when-let [deck (async/<! shuffler)]
      (recur deck))))

(defn sample-game [sample-n trump & cards]
  "ie. (sample-game 10000 nil [:clubs :king] [:clubs :2])"
  (let [play-cards (mapv #(apply create-card %) cards)
        deck (apply remove-cards (create-deck) play-cards)
        n-cards (count cards)
        other-players [:b :c :d :e]
        all-players (concat [:a] other-players )
        shuffler (create-shuffler deck sample-n)
        stats (atom {})
        n-ok (atom 0)
        table (create-table stats n-ok shuffler play-cards all-players n-cards :a trump)]
    (async/<!! table)
    {:total @n-ok :odds (into {} (map (fn [[k v]] [k (float (/ v @n-ok))]) @stats))}))

(defn test-secure-randoms [n]
  (let [sr (SecureRandom.)
        bytes (byte-array 5)]
    (loop [i 0]
      (when (< i n)
        (.nextBytes sr bytes)
        (recur (inc i))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (time (test-secure-randoms 10000000) ))

;(time (test-secure-randoms 10000000) )
