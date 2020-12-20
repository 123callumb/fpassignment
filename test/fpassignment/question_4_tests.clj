(ns fpassignment.question-4-tests
  (:require [clojure.test :refer :all]
            [fpassignment.question-4 :refer :all]
            [clojure.java.io :as io]))

; There's not many tests needed for loading the cet data in.
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

