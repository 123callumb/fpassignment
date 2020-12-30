(ns fpassignment.question-4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as spec]))

; This is so the months can be split into an array
; e.g. (:monthTemp 4) for may temp. Should be useful
; when iterating through the month calculations.
; Data structure looks like [int int int[]]
(defrecord CetRecord [year day monthTemp])
; Spec for record
(spec/def ::year int?)
; Make sure days are between 1 and 31
(spec/def ::day (spec/and int? #(and (< % 31) (> % 0))))
; Make sure that there are 12 temps, 1 for each month and they are all numbers
(spec/def ::monthTemp (spec/and vector? #(= (count %) 12) #(every? int? %)))
; Create record spec
(spec/def ::CetRecordSpec (spec/keys :req-keys [::year ::day ::monthTemp]))
; Create spec method for checking the cet array
(spec/def ::CetRecordArraySpec (fn [record] (every? #(spec/conform ::CetRecordSpec %) record)))

; Just a nice to have for displaying results
(def months ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])
; Spec for checking to make sure it is one of these
(spec/def ::monthName (spec/and string? (fn [m] (= true (some #(= m %) months)))))

; Load the cet data file into a formatted object that can be used
; in all of the other methods. Will return nil/throw a spec
; assertion error if something goes  wrong with reading.
(defn get-cet []
  ; Run spec check on call back, this will slow down all the methods,
  ; but it's nice to have to ensure the data structure is correct.
  {:post [(spec/valid? ::CetRecordArraySpec %)]}
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
; result data struct should look something like:
; { date: int, temp: int, year: int }[12]
; whilst looping through the data.
; The year isnt really needed that much here but it's
; nice to see.
(defrecord WarmestMonthDate [date temp year])
(defrecord WarmestDaysResult [monthName day temp year])
(spec/def ::temp int?)
(spec/def ::WarmestDaysResult (spec/keys :req-keys [::monthName ::day ::temp ::year]))
(spec/def ::WarmestDaysResultArray (spec/and vector? #(= (count %) 12) (fn [result] (every? #(spec/conform ::WarmestDaysResult %) result))))

(defn find-warmest-days []
  {:post [(spec/valid? ::WarmestDaysResultArray %)]}
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
        (vec (map-indexed #(WarmestDaysResult. (months %1) (:date %2) (:temp %2) (:year %2)) result))))))

; ==== Question 2 ====
; This groups each cet row by the year and then puts all of the
; temperatures from that year into one big vector and uses that
; to calculate the mean. It will do this for each year and output
; the minimum mean and maximum mean by comparing the current years
; mean with the current minimum and maximum.
(defrecord YearlyMean [year mean])
(defrecord AllYearTemps [year temps])

; This for validating the structure of the year temperature record before the recursion starts within the function
(spec/def ::mean number?)
(spec/def ::warmest (spec/keys :req [::year ::mean]))
(spec/def ::coldest (spec/keys :req [::year ::mean]))
(spec/def ::ValidateWarmestColdestResult (spec/keys :req-keys [::warmest ::coldest]))

(defn warmest-coldest-years []
  {:post [(spec/valid? ::ValidateWarmestColdestResult %)]}
  (let [cet (get-cet)
        ; Group by the year, group by seems to unorder the mapping. This shouldn't affect anything tho.
        cetYearly (group-by :year cet)
        ; Flatten the records into a structure of [year: int temps: int[]]))
        ; so the year is then assoicated with a big list of temperatures for its days.
        ; Later on this shoud be easier to run a reduce on to get the mean for the year
        yearTemps (vec (map (fn [record] (AllYearTemps. (record 0) (vec (reduce #(concat %1 (:monthTemp %2)) [] (record 1))))) cetYearly))
        ; Get the size outside the loop so it doenst have to be calculated each time
        rowAmount (count yearTemps)]
       (loop [index 0 yearlyMaxMean (YearlyMean. 0 nil) yearlyMinMean (YearlyMean. 0 nil)]
             ; Stop recursion when it's got to the end of the list
             (if (= index rowAmount)
               ; Return means in a somewhat readable data structure
               (hash-map :warmest yearlyMaxMean :coldest yearlyMinMean)
               (let [yearTemp (yearTemps index)
                     year (:year yearTemp)
                     ; Filter out the -999 temps as they're not days
                     filteredTemps (filter #(not= % -999) (:temps yearTemp))
                     ; Add all the temps up
                     tempTotal (reduce + filteredTemps)
                     ; Take the temp total and divide by the total amount of numbers
                     ; that were used in the reduce. (Need to use the filtered list here as days with -999 shouldn't be accounted for)
                     tempMean (float (/ tempTotal (count filteredTemps)))
                     ; Check to see if the mean is smaller than the current smallest and replace if it is. (Also replace if is nil from first loop)
                     newLowestMean (if (or (= (:mean yearlyMinMean) nil) (> (:mean yearlyMinMean) tempMean)) (YearlyMean. year tempMean) yearlyMinMean)
                     ; Check to see if the mean is bigger than current max mean and replace if it is. (Replace nil too)
                     newHighestMean (if (or (= (:mean yearlyMaxMean) nil) (< (:mean yearlyMaxMean) tempMean)) (YearlyMean. year tempMean) yearlyMaxMean)]
                 ; Recur until we get through every year
                 (recur (inc index) newHighestMean newLowestMean))))))

; ==== Question 3 ====
; Mean temperature of each month and the highest and lowest temp per month
; Record to make it more readable, total temp is all of the temps added
; together from a given month for every year.
; We need to see how many entires there are too as we cannot
; divide by the total amount of cet rows as it will not account for leap
; years. We can also track the lowesr and highest value in here too.
(defrecord MonthTotals [totalTemp entries lowestVal highestVal])
; This is just for the result set so it is nicely formatted
(defrecord MonthStatResults [monthName mean lower upper])
; Expecting lower and upp to be integers as they're directly from the cet data and not manipulated in any way.
(spec/def ::lower int?)
(spec/def ::upper int?)
(spec/def ::MonthStateResultSpec (spec/keys :req-un [::monthName ::mean ::lower ::upper]))
; Make sure that the :post result of the function is a vector of 12 months with each of their means, uppers, lowers and month name.
(spec/def ::MonthStateResultArraySpec (spec/and vector? #(= (count %) 12) (fn [m] (every? #(spec/conform ::MonthStateResultSpec %) m))))

(defn montlhy-average []
  {:post [(spec/valid? ::MonthStateResultArraySpec %)]}
  (let [cet (get-cet)]
    ; Catch a nil cet data just incase
    (if (= cet nil)
      (println "Failed to get CET data.")
      ; Calc the size of the cet data so its not done everytime in the loop
      (let [cetSize (count cet)]
        ; Month totals is a vecotor with a size of 12, this is for each month, e.g. 0 index is jan
        ; Index is 0, this will increase each loop as we're going row by row on the cet data.
         (loop [monthTotals (vec (repeat 12 (MonthTotals. 0 0 0 0))) index 0]
           ; If we hit the index of the size of the cet data, the loop has completed and the
           ; function goes onto returning the stats collected.
           (if (= index cetSize)
             (vec (map-indexed            ; Just to look nice I add the month name into the print log
                    (fn [index val] (let [monthName (months index)
                                          ; The mean is calculated at the end of the loop, the total temp is
                                          ; divided by the total amount of entires for that month.
                                          ; There is probably above 7000 entires for each month since 1775.
                                          ; Formatted twice into a float here so it is rounded and still a number in the
                                          ; result set.
                                          meanVal (Float/parseFloat (format "%.2f" (float (/ (:totalTemp val) (:entries val)))))
                                          ; Here just grab the lowest val and highest val,
                                          ; these values were already concluded in the loop.
                                          lowestVal (:lowestVal val)
                                          highestVal (:highestVal val)]
                                      ; Place it into this record format so its nice and organised.
                                      (MonthStatResults. monthName meanVal lowestVal highestVal))) monthTotals))
             ; First grab the row to get its readings
             (let [cetRow (cet index)
                   ; This will return a new array of monthTotals that have accounted for the new cet row.
                   ; This map runs through each month... so 12 times. Map indexed is used so we can use the
                   ; index value to get the relevant month.
                   newTotals (vec (map-indexed
                                    ; monthTemp here grabs the month temperature from the cet row, the cet row has
                                    ; an array of months stored under the :monthTemp key. So it gets the index based
                                    ; on the current mapping index.
                                    #(let [monthTemp ((:monthTemp cetRow) %1)]
                                       ; Catch results the are -999, this means that there is no data for that day becuase
                                       ; it may not be a date that exists in the given month, e.g. 31st feb would return -999.
                                       ; In this instance the exisitng value is returned and then it moves onto the next month.
                                       (if (= monthTemp -999)
                                         %2
                                         ; Total temp is the current temp total in the months total vector added onto
                                         ; the current temperature. There is also an increase in the entries needed here
                                         ; and checking to see if the current temp qualifies for the lowest of the month
                                         ; or the highest.
                                         (let [totalTemp (+ monthTemp (:totalTemp %2))
                                               totalEntries (inc (:entries %2))
                                               lowest (if (< monthTemp (:lowestVal %2)) monthTemp (:lowestVal %2))
                                               highest (if (> monthTemp (:highestVal %2)) monthTemp (:highestVal %2))]
                                           ; Returns the MonthTotals record format.
                                           (MonthTotals. totalTemp totalEntries lowest highest))))
                                    monthTotals))]
               ; Once the all 12 indexes have been updated and account for the current cet row, the index is then increased
               ; and the loop recurs for the next cet row :)
               (recur newTotals (inc index)))))))))

; ==== Question 4 ====
; Find the average temperature for any given day based
; on previous years for that day.
; Enter a string with the format dd-MM
; e.g. the 4th of May will be; 04-05 or 4-5
(spec/def ::date (spec/and string? #(= 2 (count (str/split % #"-")))))

(defn avg-day-temp [date]
  {:pre [(spec/valid? ::date date)]
   :post [(spec/valid? number? %)]}
  (let [dateSplit (str/split date #"-")]
      (let [day (Integer/parseInt (dateSplit 0))
            month (Integer/parseInt (dateSplit 1))]
        (if (or (or (> month 12) (< month 1)) (or (< day 1) (> day 31)))
          (throw (IllegalArgumentException. "Please enter a valid day range (1 - 31) and a valid month range  (1 - 12)"))
          (let [cet (get-cet)]
            (if (= cet nil)
              (throw (Exception. "Issue getting cet data."))
              (let [cetFiltered (filter #(= (:day %) day) cet)
                    ; Grab the temperatures for every year at the chosen month
                    ; put them in a map so we can count the entries
                    ; Have to negate 1 from the month because months are 0 index in
                    ; the cet data.
                    forMonth (map #((:monthTemp %) (dec month)) cetFiltered)
                    ; Fitler out -999 days incase we're looking at 30th of feb
                    ; this can be used to see if a correct date was entered too!
                    filterNonDays (filter #(not= % -999) forMonth)
                    tempEntries (count filterNonDays)]
                (if (= tempEntries 0)
                  (throw (RuntimeException. "Could not find any temperatures for the date given. Did you try Feburary 30th? How cheeky..."))
                  (let [avgTemp (float (/ (reduce + filterNonDays) tempEntries))]
                    (println "The average temperature for " date " is " avgTemp)
                    avgTemp)))))))))

; === Question 5 ===
; This function predicts if global warming is real in a naive way.
; It uses multithreading to calculate the average temperature of each year.
; Then it runs a regression analysis function on the data which returns
; the gradient of a line. This represents a linear trend in the data that
; indicates if the temperatures are increasing or decreasing.
; If the value is negative then our temperatures are decreasing, if it
; returns a positive number then the temperatures are increasing.
; This in no way will end up representing real world global warming, but
; I thought it may be fun function to write.

; Calculate the average mean for a cet year grouping.
(defn calc-yearly-average [cetGrouping]
  (let [year (cetGrouping 0)
        ; This gets every temperature of for the year and filters out any -999 values
        yearTemps (reduce (fn [totalTemps record] (concat totalTemps (filter #(not= % -999) (:monthTemp record)))) [] (cetGrouping 1))
        ; Divides by the amount of temps in the list above, this is important, as just dividing by 365 would not be a true average, and
        ; also would not account for a leap year
        yearAvg (float (/ (reduce + yearTemps) (count yearTemps)))]
    ; return the value in record format (Record was created further up the page for a previous question)
    (YearlyMean. year yearAvg)))

; This calculates a regression analysis formula on a given xy dataset.
; This willl return 'a' from the equation of a line y = ax+c
; The gradient will determine a positive or negative increase of temperature)
; I've added keywords for x and y properties to maek the find regression a little more
; generic
(defn find-regression-gradient [xy xKeyword yKeyword]
  (let [xKey (keyword xKeyword)
        yKey (keyword yKeyword)
        rows (count xy)
        xVals (map #(xKey %) xy)
        yVals (map #(yKey %) xy)
        xMean (/ (reduce + xVals) rows)
        yMean (/ (reduce + yVals) rows)
        xMinusMean (vec (map #(- % xMean) xVals))
        yMinusMean (vec  (map #(- % yMean) yVals))
        xMinusMeanSqr (map #(* % %) xMinusMean)
        xyMinusMean (map-indexed #(* %2 (xMinusMean %1)) yMinusMean)
        bottomTotal (reduce + xMinusMeanSqr)
        topTotal (reduce + xyMinusMean)]
    (float (/ topTotal bottomTotal))))


(defn is-the-global-warming-multi-threaded
  []
  (let [cet (get-cet)
        ; Group the data by the year
        groupedYears (group-by :year cet)
        ; Using futures here seems the best as they are automatically create
        ; a thread and run it.
        futureCalcs (vec (map #(future (calc-yearly-average %)) groupedYears))]
    ; Wait until every future has finished and calculted a result.
    (while (not-every? #(future-done? %) futureCalcs))
    ; Map all of the values from the futures be dereferencing it,
    (let [yearAvgs (map #(deref %) futureCalcs); @% instead of deref doesn't work here for some reason?
          orderedAvgs (sort-by :year yearAvgs) ; order by year so we can see the trend based on the increase of time
          lineGradient (find-regression-gradient orderedAvgs "year" "mean")]
      (> lineGradient 0)))) ; less than 0 is a negative linear line and above is positive. If true - the globe is warming, oh no! :P

; Did in single threading just to see if there is a performace gain.
; Analysed in the unit tests.
(defn is-the-global-warming-single-threaded
  []
  (let [cet (get-cet)
        groupedYears (group-by :year cet)
        yearAvgs (vec (map #(calc-yearly-average %) groupedYears)); @% instead of deref doesn't work here for some reason?
        orderedAvgs (sort-by :year yearAvgs) ; order by year so we can see the trend based on the increase of time
        lineGradient (find-regression-gradient orderedAvgs "year" "mean")]
      (> lineGradient 0)))