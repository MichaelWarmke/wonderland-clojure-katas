(ns alphabet-cipher.coder
  (:require [clojure.string :as str]))

(def alpha "abcdefghijklmnopqrstuvwxyz")

(defn normalizeOffset [offset max]
  (cond
    (>= offset max) (normalizeOffset (- offset max) max)
    (< offset 0) (normalizeOffset (+ offset max) max)
    :else offset))

(defn calcOffset [mode max base offset]
  (cond
    (= mode "e") (normalizeOffset (+ base offset) max)
    (= mode "d") (normalizeOffset (- base offset) max)
    :else nil))

(defn trans [mode keyword message]
  (let [keywordChars (take (count message) (cycle (str/split keyword #"")))
        messageChars (str/split message #"")]
    (loop [k keywordChars
           m messageChars
           buf ""]
      (if (or (empty? m) (empty? k))
        buf
        (let [k-offset (str/index-of alpha (first k))
              m-offset (str/index-of alpha (first m))
              offset (calcOffset mode (count alpha) m-offset k-offset)]
          (recur (rest k) (rest m) (str buf (get alpha offset))))))))

(defn encode [keyword message]
  (trans "e" keyword message))

(defn decode [keyword message]
  (trans "d" keyword message))

(defn findDuplicate [repeatedKeyword]
    (loop [subsSize 1]
      (let [matchTerm (subs repeatedKeyword 0 subsSize)
            matched (re-find (re-pattern (str "(" matchTerm ")+")) repeatedKeyword)]
        (if (>= (count (first matched)) (/ (count repeatedKeyword) 2))
          (last matched)
          (recur (inc subsSize))))))

(defn decipher [cipher message]
  (findDuplicate (decode message cipher)))

