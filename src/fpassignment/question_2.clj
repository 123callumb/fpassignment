(ns fpassignment.question-2
  (:require [clojure.spec.alpha :as spec]))

; === 2. COIN COMBINATIONS ===
; EXPLANATION:
; To do this I based the algorithm around the idea of calculating the previous amount of combinations
; from n-1 of the target and storing it in a 2d array with the combination count whilst looping through
; the coin collection.

; Collection of coins just to use for the repl
(def us-coins [1 5 10 25 50 100])

; For the spec. Need to make sure that the list of coins is a vector
; of positive integers, not any other type of number/type.
(spec/def ::coins-are-valid (spec/and vector? #(every? (fn [coin] (and (int? coin) (> coin 0))) %)))

(defn calc-coin-combinations
  ; Take target and coins so we can change the values
  [target coins]
  {:pre [(spec/valid? ::coins-are-valid coins)
         ; Make sure the target is also an int
         (spec/valid? (spec/and int? #(> % 0)) target)]
   ; make sure that the final result will be an integer.
   :post [(spec/valid? int? %)]}
  ; This is the size that the combination array will be.
  (let [combinationSize (inc target)
        ; Make sure coins are distinct incase of parameter input error.
        distinctCoins (vec (distinct coins))]
    ; Combinations: Prefill the vector so we don't have to deal with any additional index not found issues.
    ; Index: This is the current target value. This increases one by one as we go across the
    ; combination array. This will reset to 0 everytime we reach the end of the combination array.
    ; CoinIndex: This is the current coin we are fitting into the element at the combination array index. This will
    ; increase everytime the index resets to 0.
    (loop [combinations (vec (repeat combinationSize 0)) index 0 coinIndex 0]
      ; Check to see if the coin index is equal to the amount of coins. If so then we have reached the end
      ; of the coin list and the end of the function, so we can return the value at the end of the combination
      ; array.
      (if (= coinIndex (count distinctCoins))
        (combinations target)
        ; This is to catch when the index reaches the end of the combination array so we can reset the index
        ; and increase the coin value
        (if (not= index combinationSize)
          ; I'll modify this once I've done all the other questions, this is to catch the initial call to ensure that
          ; index 0 is equal to 1, this will be different when the coin array does not contain a 1. It will need to
          ; equal 0 and the 1 should be placed at the lowest coin value, which is also equal to the combination index.
          ; e.g. a coin list of 3, 4, 5 would mean we start at combination index 3 where 1 would be added, as totals of
          ; 0, 1, 2 cannot be gotten from the given coins
          (if (not= (+ index coinIndex) 0)
            (let [coinVal (distinctCoins coinIndex)]
              ; This is to catch when the current coin value is more than the current target index. This means that the
              ; coin cannot fit into the current target so we must increase the target index by 1.
              (if (>= index coinVal)
                ; CombIndex: This is the target index negated from the current coin, this will leave us with a remaining
                ; value.
                ; CombIndexVal: This is the amount of combinations the remaining value already has, we can get this from
                ; the combination array.
                ; CombIndexTotal: We then add the amount of combinations of the remaining value to the current amount
                ; of combinations at the current target index.
                (let [combIndex (- index coinVal)
                      combIndexVal (combinations combIndex)
                      combIndexTotal (+ combIndexVal (combinations index))]
                  (recur (assoc combinations index combIndexTotal) (inc index) coinIndex))
                (recur combinations (inc index) coinIndex)))
            (recur (assoc combinations 0 1) (inc index) coinIndex))
          (recur combinations 0 (inc coinIndex)))))))