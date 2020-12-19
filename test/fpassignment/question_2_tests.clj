(ns fpassignment.question-2-tests
  (:require [clojure.test :refer :all]
            [fpassignment.question-2 :refer :all]))

; Why isn't exponent a function already clojure!!!
(defn exp [x n] (reduce * (repeat n x)))

(deftest coin-counting-tests
  (testing "Invalid coin vector supplied - Assertion Error Thrown"
    (is (thrown? AssertionError (calc-coin-combinations 100 [1 -55 "hi"]))))
  (testing "Invalid targert - Assertion Error Thrown"
    (is (thrown? AssertionError (calc-coin-combinations "Not a target" [1 5 10]))))
  (testing "Combinations for coins 1, 5, 10, 25 for $1"
    (let [coins [1 5 10 25]
          target 100
          result (time (calc-coin-combinations target coins))]
      (println "TEST: " result " combinations.")))
  (testing "Combinations for coins 1, 5, 10, 25, 50, 100 for $1"
   (let [coins [1 5 10 25 50 100]
         target 100
         ; Googled this expected result so I could do an assertion on it
         expectedResult 293
         result (time (calc-coin-combinations target coins))]
     (println "TEST: " result " combinations.")
     (is (= result expectedResult))))
  (testing "Combinations for coins 1, 5, 10, 25, 50, 100 for $1000"
    (let [coins [1 5 10 25 50 100]
          target 100000
          result (time (calc-coin-combinations target coins))]
      (println "TEST: " result " combinations.")
      (is (> result (exp 2 32))))))