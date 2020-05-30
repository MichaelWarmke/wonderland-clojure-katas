(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(def royalNumbers {:jack 11
                   :queen 12
                   :king 13
                   :ace 14})

(defn getRank [card]
  (let [rank (last card)]
    (if (keyword? rank)
      (get royalNumbers rank)
      rank)))

(defn play-round [player1-card player2-card]
  (let [rank1 (getRank player1-card)
        rank2 (getRank player2-card)]
    (cond
      (== rank1 rank2) {:war true :cards [[player1-card player2-card]]}
      (> rank1 rank2) {:player1 true :cards [player1-card player2-card]}
      (< rank1 rank2) {:player2 true :cards [player1-card player2-card]}
      :else {})))

(defn play-game [player1-cards player2-cards]
  (loop [cards1 player1-cards
         cards2 player2-cards
         leftOverPool []]
    (let [restCards1 (drop 1 cards1)
          restCards2 (drop 1 cards2)
          card1 (first cards2)
          card2 (first cards2)
          result (play-round card1 card2)]
      (println "player1 count: " (count cards1) " player2 count: " (count cards2) " Pool: " (count leftOverPool))
      (cond
        (:war result) (recur (drop 3 restCards1) (drop 3 restCards2) (conj (take 3 restCards1) (take 3 restCards2)))
        (:player1 result) (recur (conj restCards1 (:cards result) leftOverPool) restCards2 [])
        (:player2 result) (recur restCards1 (conj restCards2 (:cards result) leftOverPool) [])
        :else (println "Game Over.")))))

(getRank [:club :ace])
(play-round [:club 2] [:spade 2])
(let [[p1 p2] (split-at (/ (count cards) 2) (shuffle cards))]
  (play-game p1 p2))

(println (count cards))
(drop 1 [[:club :ace]])

