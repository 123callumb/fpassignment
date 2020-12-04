(ns fpassignment.question-4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

; This is so the months can be split into an array
; e.g. (:monthTemp 5) for may temp. Should be useful
; when iterating through the month calculations.
; Data structure looks like [int int int[]]
(defrecord CetRecord [year day monthTemp])

; Load the cet data file into a formatted object that can be used
; in all of the other methods. Will return nil if something goes
; wrong with reading. Other functions should check for this.
(defn get-cet []
  (try (let [file (slurp (io/resource "cetdl1772on.dat"))
             ; Split line by line for each day entry
             lines (str/split file #"\n")
             ; Trim the whitespaces infront and at the end of the lines.
             trimedLines (map #(str/trim %) lines)
             ; Seperate into a vecotor based on the spaces
             formattedData (map #(str/split % #"\s+") trimedLines)
             ; Place into a record so it is more readable
             asRecords (map #(apply ->CetRecord [(% 0) (% 1) (subvec % 2)]) formattedData)]
         asRecords)
       (catch Exception ex (println "Exception: " (.toString ex)))
       (finally nil)))

; === Question 1 ===
; Warmest day for each calendar month
; loop through each record
(defn find-warmest-days []
  (let [cet (get-cet)]))