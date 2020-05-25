(ns alphabet-cipher.coder
  (:require [clojure.string :as str]))

(def alpha "abcdefghijklmnopqrstuvwxyz")

(defn circularOffset [offset max]
  (if (>= offset max)
    (- offset max)
    offset))

(defn encode [keyword message]
  (let [keywordChars (take (count message) (cycle (str/split keyword #"")))
        messageChars (str/split message #"")]
    (loop [k keywordChars
           m messageChars
           buf ""]
      (if (or (empty? m) (empty? k))
        buf
        (let [k-offset (str/index-of alpha (first k))
              m-offset (str/index-of alpha (first m))
              offset (circularOffset (+ k-offset m-offset) (count alpha))]
          (recur (rest k) (rest m) (str buf (get alpha offset))))))))

(defn decode [keyword message]
  "decodeme")

(defn decipher [cipher message]
  "decypherme")

