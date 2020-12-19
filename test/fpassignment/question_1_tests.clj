(ns fpassignment.question-1-tests
  (:require [clojure.test :refer :all]
            [fpassignment.question-1 :refer :all]))

; Thrown here is like a macro in clojure and the cursive intelij plugin
; does not like it as it's treating it like a function. This still works completely
; fine, however, intellij likes to highlight the syntax with a warning.
; This still runs as expected at runtime.
; Bit of info on that here: https://github.com/cursive-ide/cursive/issues/238#issuecomment-705029202

(deftest q1-non-spec-validation
  (testing "Paramter is not a vector - Throws Assertion Error"
    (is (thrown? AssertionError (square-list 12))))
  (testing "Parameter contains a non number"
    (let [param [1 2 "hi" 4]
          resultAsVec (vec (square-list param))]
      (is (= (resultAsVec 2) "NaN") "Result should contain a NaN at the same index")))
  (testing "Success, full list of numbers"
    (is (every? number? (square-list [1 2 3 4])))))


(deftest q1-spec-validation
  (testing "Parameter contains non numberin list - Throws Assertion Error"
    (is (thrown? AssertionError (square-list-spec-validation [1 2 "hi" 3]))))
  (testing "Success, full list of numbers"
    (is (every? number? (square-list-spec-validation [1 2 3 4])))))