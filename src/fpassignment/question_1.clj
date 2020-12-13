(ns fpassignment.question-1
  (:require [clojure.spec.alpha :as spec]))

; === 1. SQUARING LISTS ===
; EXPLANATION:
; The function will take in a vector filled with any type of different elements, it will
; run a map function on the vector. Here I have opted for a lamda as the
; function is not too complicated and shouldn't require separating out.
; All elements that are classified as a number will be multiplied by
; theirself, ones that clojure does not recognise as a number will return
; NaN (Not a Number). The result will reutrn a lazy sequence as the map
; function returns one by default.
(defn square-list
  [list]
  ; Use spec to make sure that the list passed in is a vector
  {:pre [(spec/valid? vector? list)]}
  (map #(if (number? %) (* % %) "NaN") list))

; Using spec and expecting the list passed in to all be numbers instead of the
; previous way where non numbers produce a NaN in the result list.
(spec/def ::list-is-nums #(every? number? %))
(spec/def ::is-list-of-nums (spec/and vector? ::list-is-nums))
(defn square-list-spec-validation
  [list]
  {:pre [(spec/valid? ::is-list-of-nums list)]}
  (map #(* % %) list))

