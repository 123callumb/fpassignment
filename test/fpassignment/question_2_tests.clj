(ns fpassignment.question-2-tests
  (:require [clojure.test :refer :all]
            [fpassignment.question-2 :refer :all]))

(deftest coin-counting-tests
  (testing "Invalid coin vector supplied - Assertion Error Thrown"
    (is (thrown? AssertionError (calc-coin-combinations 100 [1 -55 "hi"]))))
  (testing "Invalid targert - Assertion Error Thrown"
    (is (thrown? AssertionError (calc-coin-combinations "Not a target" [1 5 10])))))
