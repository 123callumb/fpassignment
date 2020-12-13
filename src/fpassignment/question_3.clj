(ns fpassignment.question-3
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as spec]))

; === 3. KINDERGARDEN  ===
; EXPLANATION:
; First the function checks to make sure the child is in the kindergarden class, it does this with specs and
; also the function itself. The function grabs the childs index once ordering the list alphabetically. It uses
; this index and multiplies it by 2 as each child has two seeds per row. It then gets the item at this index
; in the garden. It will use this item to call the find-seed function. This function simply gets the seed that
; starts with character passed into it.

; Here are the constants at play
(def children ["Alice" "Bob" "Charlie" "David" "Eve" "Fred","Ginny" "Harriet" "Ileana" "Joseph" "Kincaid" "Larry"])
(def seeds ["violets" "clover" "radishes" "grass"])
(def garden ["VRCGVVRVCGGCCGVRGCVCGCGV" "VRCCCGCRRGVCGCRVVCVGCGCV"])
; Generic spec string list helper
(spec/def ::is-string-list #(every? string? %))

; There is a catch in the function already for children that are not found, but this spec adds a bit of
; extra validation, ensuring that the child name passed into the function is part of the child array.
(spec/def ::valid-child #(some (fn [childName] (= (.toLowerCase %) (.toLowerCase childName))) children))

; Make sure that the seed character that is passed in exsits in the seed list, and the garden has not
; added any new seeds.
(defn validate-seed
  [seed]
  {:pre [(spec/valid? char? seed)]}
  (let [seedChars (map #(.toLowerCase (subs % 0 1)) seeds)
        seedAsStr (.toLowerCase (str seed))]
    (some #(= % seedAsStr) seedChars)))
(spec/def ::valid-seed validate-seed)

; Function that takes a character and returns the relevant seed that
; begins with that character. Will return null if the seed is not found.
(defn find-seed [character]
  {:pre [(spec/valid? ::valid-seed character)]}
  ; Lower case character and turn it into a string if it is passed in as a char (easier for predicate)
  (let [charLwr (.toLowerCase (str character))]
    (first (filter #(str/starts-with? % charLwr) seeds))))

; Main function here:
(defn childrens-plants [childName]
  {:pre [(spec/valid? ::is-string-list seeds)
         (spec/valid? ::valid-child childName)
         (spec/valid? ::is-string-list garden)]}
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