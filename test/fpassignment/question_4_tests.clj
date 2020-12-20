(ns fpassignment.question-4-tests
  (:require [clojure.test :refer :all]
            [fpassignment.question-4 :refer :all]
            [clojure.java.io :as io]))

; There's not many tests needed for quite a few of these functions
; as their output is already check by spec and the first three
; questions do not take any arguments in.
; The method has spec already applied to it that ensures the
; data structure returned is correct.
(deftest get-cet-data-tests
  (testing "Load data time test"
    (is (not= nil (time (get-cet)))))
  (testing "File content is incorrect"
    (let [file (io/resource "cetdl1772on.dat")
          fileContent (slurp file)]
      ; Wipe the file before the function is called
      (spit file "" :append false)
      ; Expect to return nil as the function should catch the format exception
      (is (= nil (get-cet)))
      ; Put the original content back into the file
      (spit file fileContent :append false))))

(deftest find-warmest-days-tests
  (testing "Find warmest days time taken"
    (is (not= nil (time (find-warmest-days))))))

(deftest warmest-coldest-years-tests
  (testing "Find warmest and coldest time taken"
    (let [result (time (warmest-coldest-years))]
      (are [res ags]
        (not= nil result)
        (not= nil (:coldest result))
        (not= nil (:warmest result))))))

(deftest montlhy-average-tests
  (testing "Monthly averages time taken"
    (is (not= nil (time (montlhy-average))))))

(deftest avg-day-temp-tests
  (testing "Invalid date format entered - Throws Assertion Error"
    (is (thrown? AssertionError (avg-day-temp "05/55"))))
  (testing "Invalid day - Throws Illegal Arguement Exception"
    (is (thrown? IllegalArgumentException (avg-day-temp "33-05"))))
  (testing "Invalid month - Throws Illegal Arguement Exception"
    (is (thrown? IllegalArgumentException (avg-day-temp "15-14"))))
  (testing "Invalid month and day - Throws Illegal Arguement Exception"
    (is (thrown? IllegalArgumentException (avg-day-temp "40-120"))))
  (testing "Invalid date formated entered with year - Throws Assertion Error"
    (is (thrown? AssertionError (avg-day-temp "04-05-2020"))))
  (testing "Feburary 30th, date not found - Throws Runtime Error"
    (is (thrown? RuntimeException (avg-day-temp "30-02"))))
  (testing "Success - Valid date- with time taken"
    (is (not= nil (time (avg-day-temp "27-05")))))
  (testing "Success - Format without zeros"
    (is (not= nil (avg-day-temp "1-4")))))