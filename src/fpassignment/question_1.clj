(ns fpassignment.question-1)

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
  (map #(if (number? %) (* % %) "NaN") list))
; This could be a trick question I feel like there should be more than this...