(ns fpassignment.question-3
  (:require [clojure.string :as str]))

; === 3. KINDERGARDEN  ===
; EXPLANATION:
;
(def children ["Alice" "Bob" "Charlie" "David" "Eve" "Fred","Ginny" "Harriet" "Ileana" "Joseph" "Kincaid", "Larry"])
(def seeds ["violets" "clover" "radishes" "grass"])
(def garden ["VRCGVVRVCGGCCGVRGCVCGCGV" "VRCCCGCRRGVCGCRVVCVGCGCV"])

; Function that takes a character and returns the relevant seed that
; begins with that character. Will return null if the seed is not found.
(defn find-seed [character]
  ; Lower case character and turn it into a string if it is passed in as a char (easier for predicate)
  (let [charLwr (.toLowerCase (str character))]
    (first (filter #(str/starts-with? % charLwr) seeds))))

(defn childrens-plants [childName]
  ; lowercase and order by just incase the child list has been
  ; messed with.
  (let [childrenLwr (map #(.toLowerCase %) (sort children))
        child (.toLowerCase childName)
        ; Maybe improve this magic number 2 to be based off garden row length/no. of children
        seedIndex (* (.indexOf childrenLwr child) 2)]
    ; Seed index will return -2 if index is not found for the child
    (if (> seedIndex -1)
      ; Using reduce so there can be x amount of rows not just 2. Each row get the character that is at
      ; the same index as the child. This index is multiplied by 2 as each child has 2 seeds per row.
      ; The index and index +1 is used against the garden row to get their seeds next to one another.
      (let [seedChars (reduce #(into %1 [(get %2 seedIndex) (get %2 (inc seedIndex))]) [] garden)]
        (map #(find-seed %) seedChars))
      (print "Could not find child in the kindergarden. We have either lost them or you came to the wrong one."))))