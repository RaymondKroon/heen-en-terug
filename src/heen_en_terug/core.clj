(ns heen-en-terug.core
  (:require [clojure.core.async :as async]
            [clojure.java.jdbc :as jdbc])
  (:import [java.security SecureRandom])
  (:gen-class))

(def secure-generator ^SecureRandom (SecureRandom.))

(defn secure-shuffle
  "Return a random permutation of coll"
  [^java.util.Collection coll]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al ^SecureRandom secure-generator )
    (clojure.lang.RT/vector (.toArray al))))

(defrecord Card [id suit rank name])

(defmethod print-method Card [v ^java.io.Writer w]
  (.write w (str (.suit v) (.name v))))

(def all-suits [:spades :clubs :hearts :diamonds])
(def all-cards {:2 0, :3 1, :4 2, :5 3, :6 4, :7 5, :8 6, :9 7, :10 8
            :jack 9, :queen 10, :king 11, :ace 12})

(defn encode-suit [suit]
  (get {:spades 1 :clubs 2 :hearts 3 :diamonds 4} suit 0))

(defn create-card [suit name]
  (let [rank (get all-cards name  -999)]
    (->Card (+ rank (* 13 (encode-suit suit))) suit rank name)))

(defn create-deck []
  (for [suit all-suits
        [name _] all-cards]
    (create-card suit name)))

(def id->card
  (let [idx (into {} (map #(vector (:id %1) %1) (create-deck)))]
    (fn [id] (get idx id))))

(def card->id
  (let [idx (into {} (map #(vector [(:suit %1) (:name %1)] (:id %1)) (create-deck)))]
    (fn [suit name] (get idx [suit name]))))

(defn remove-card [deck card]
  (filter (fn [c] (not= card c)) deck))

(defn remove-cards [deck & cards]
  (loop [[card & more-cards] cards
         remaining-deck deck]
    (if card
      (recur more-cards (remove-card remaining-deck card))
      remaining-deck)))

(defn create-shuffler [deck shuffle-time]
  "returns channel which returns shuffled decks for shuffle-time seconds"
  (let [c (async/chan 1000)
        has-time? (volatile! true)]
    (async/go
      (async/<! (async/timeout (* 1000 shuffle-time)))
      (vreset! has-time? false))
    (future
      (while @has-time?
        (async/>!! c (secure-shuffle deck)))
      (async/close! c))
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

(defn valid-play? [requested-suit trump winning-card played-card remaining-cards]
  (cond
    (and (not (requested-suit (:suit played-card)))
         (some requested-suit (map :suit remaining-cards))) false
    (and (not (requested-suit trump))
         (= trump (:suit winning-card))
         (= trump (:suit played-card))
         (> (:rank winning-card) (:rank played-card))
         (not (every? #(and (= trump (:suit %1))
                            (> (:rank winning-card) (:rank %1))) remaining-cards))) false
    :else true))

(defn play-trick [players trick-i starting-player players-cards trump]
  "return winning player, nil if invalid round"
  (let [order (take (count players) (drop-while #(not= % starting-player) (cycle players)))
        first-player (first order)
        ;_ (print first-player)
        first-card  (-> players-cards (get first-player) (get trick-i))
        requested-suit #{(:suit first-card)}]
    (loop [[player & more-players] (drop 1 order)
           winning-player-card [(first order) first-card]
           valid? true]
      (if (and valid? player)
        (let [player-cards (get players-cards player)
              played-card (get player-cards trick-i)
              nw-player-card (compare-cards winning-player-card [player played-card] trump)
              nw-valid? (and valid?
                             (or (= player (first nw-player-card))
                              (valid-play? requested-suit trump (second winning-player-card) played-card
                                           (drop 1 player-cards))))]
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
      (if-let [[player _] (play-trick players trick-i starting-player player-cards trump)]
        (recur (inc trick-i) (update-in tricks-won [player] (fnil inc 0)) player)
        nil)
      tricks-won))
  )

(defn position? [n-players starting-player player]
  (+ 1 (mod (- player starting-player) n-players)))

(defn play-table [shuffler players n-cards trump]
  (let [c (async/chan 1000)]
    (future
      (loop [deck (async/<!! shuffler)]
        (let [player-cards (deal deck players n-cards)]
          (doseq [starting-player players]
            (when-let [game-result (play-game players n-cards starting-player player-cards trump)]
              (doseq [p players]
                (async/>!! c {:trump trump
                              :position (position? (count players) starting-player p)
                              :cards (get player-cards p)
                              :tricks (get game-result p 0)}))))
          (if-let [deck (async/<!! shuffler)]
            (recur deck)
            (async/close! c)))))
    c))

(defn create-players [n]
  (range 0 n))

(defn apply-game [stats {:keys [trump position cards tricks]}]
  (let [k [trump position cards]]
    (-> stats
        (update-in [k :per-trick tricks] (fnil inc 0))
        (update-in [k :total] (fnil inc 0)))))

(def results-db {:subprotocol "sqlite"
                 :subname "result-data/db.sqlite"})


(defn chan->aggr [play-chan]
  (let [results (async/<!!
                  (async/reduce
                    (fn [stats game-result]
                      (apply-game stats game-result))
                    {} play-chan))]
    results))

(defn encode-cards [cards]
  (clojure.string/join "," (map :id cards)))

(defn chan->db [play-chan n-players db]
  ;beter reduces, zodat er maar 1 keer per run weggeschreven hoeft te worden
  (let [stats (chan->aggr play-chan)
        entries (mapcat
                  (fn [[[trump position cards] {:keys [per-trick total]}]]
                    (map (fn [[trick v]]
                           {:n_players n-players
                            :trump (encode-suit trump)
                            :position position
                            :cards (encode-cards cards)
                            :tricks trick
                            :tricks_occurrences v
                            :total_occurrences total})
                         per-trick))
                  stats)]
    (apply jdbc/insert! db :play_result entries))
  :ok)

(defn sample-all [sample-time n-cards n-players]
  (let [deck (create-deck)
        players (create-players n-players)
        shuffler (create-shuffler deck sample-time)
        games (async/merge (doall
                            (map #(play-table shuffler players n-cards %)
                                 [nil :spades :clubs :hearts :diamonds])) 1000)]
    (chan->db games n-players results-db)))

(defn get-odds [db n-players trump & cards]
  (let [cards* (encode-cards (mapv #(apply create-card %) cards))
        trump* (encode-suit trump)
        results (jdbc/query db ["select
  position,
  tricks,
  sum(tricks_occurrences) n,
  sum(total_occurrences) total
from play_result
where trump = ?
and cards = ?
and n_players = ?
group by trump, position, cards, tricks" trump* cards* n-players])]
    (reduce (fn [acc {:keys [position tricks n total]}]
              (-> acc
                  (assoc-in [:odds position tricks] (float (with-precision 4 (/ (BigDecimal. n) total))))
                  (assoc-in [:sample-size position] total)))
            {:cards cards :trump trump :n-players n-players} results)))

;; broken
(defn sample-game [sample-time trump & cards]
  "ie. (sample-game 10000 nil [:clubs :king] [:clubs :2])"
  (let [play-cards (mapv #(apply create-card %) cards)
        deck (apply remove-cards (create-deck) play-cards)
        n-cards (count cards)
        other-players [:b :c :d :e]
        all-players (concat [:a] other-players )
        shuffler (create-shuffler deck sample-time)
        games (async/merge
               (repeatedly 1 #(play-table shuffler play-cards all-players n-cards :a trump)) sample-time)
        [n-ok stats] (async/<!!
                      (async/reduce
                       (fn [[n-ok stats] game-result]
                         [(inc n-ok) (merge-with + stats {(get game-result :a 0) 1})])
                       [0 {}]
                       games))]
    {:total n-ok :odds (into {} (map (fn [[k v]] [k (float (/ v n-ok))]) stats))}))

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
