(ns fpassignment.question-4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

; This is so the months can be split into an array
; e.g. (:monthTemp 4) for may temp. Should be useful
; when iterating through the month calculations.
; Data structure looks like [int int int[]]
(defrecord CetRecord [year day monthTemp])

; Just a nice to have for displaying results
(def months ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])

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
             splitData (vec (map #(str/split % #"\s+") trimedLines))
             ; Format data into integers, had to use double predicate here, It's ugly but i really didnt want to
             ; do it outside of this.
             formattedData  (map (fn [line] (vec (map #(Integer/parseInt %) line))) splitData)
             ; Place into a record so it is more readable
             asRecords (vec (map #(apply ->CetRecord [(% 0) (% 1) (subvec % 2)]) formattedData))]
         asRecords)
       (catch Exception ex (println "Exception: " (.toString ex)))
       (finally nil)))

; === Question 1 ===
; Warmest day for each calendar month
; loop through each record
; result data struct should look somehting like:
; { date: int, temp: int, year: int }[12]
; The year isnt really needed that much here but it's
; nice to see.
(defrecord WarmestMonthDate [date temp year])

; I think this works, im not sure how to verify..... need to ask.
(defn find-warmest-days []
  (let [cet (get-cet)]
    ; Null check just incase, can't hurt
    (if (= cet nil)
      (println "Something went wrong with reading the Cet data file :(")
      ; warmest-months: This is the initial value, the index of the array represents the month,
      ; e.g. (warmest-months 5) would be june.
      ; I will be updating the date and year value as it goes along just to see which year and
      ; date was the warmest.
      ; Index: is the position in the cet data set we're looking at
      (let [warmest-months (vec (repeat 12 (WarmestMonthDate. 0 -999 0)))
            ; I've used reduce here.. well becuase I felt like it, I'd image a loop could produce
            ; the same outcome.
            ; They way it works is by going row by row on the CET data and looping 12 times inside
            ; for each month, comparing the temperature of that month with the one from the warmest
            ; months array, if the temperate of this one is higher or equal it will replace the one
            ; from warmest months array.
            result (reduce (fn [warmestMonths dateRecord]
                             (vec (map-indexed (fn [monthIndex warmestVal]
                                                 (let [currentTemp ((:monthTemp dateRecord) monthIndex)
                                                       currentWarmestTemp (:temp warmestVal)]
                                                   (if (>= currentTemp currentWarmestTemp)
                                                     (WarmestMonthDate. (:day dateRecord) currentTemp (:year dateRecord))
                                                     warmestVal)))
                                       warmestMonths)))
                           warmest-months cet)]
        (map-indexed #(println (months %1) ": " %2) result)))))
