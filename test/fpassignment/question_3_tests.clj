(ns fpassignment.question-3-tests
  (:require [clojure.test :refer :all]
            [fpassignment.question-3 :refer :all]))

(deftest validate-seed-tests
  (testing "Seed is not a char - Throws Assertion Error"
    (is (thrown? AssertionError (validate-seed "This is not a char"))))
  (testing "Seed does not exist in seed vector"
    (is (false? (validate-seed \h))))
  (testing "Seed is in vector"
    (is (validate-seed \v))))

(deftest find-seed-tests
  (testing "Seed given is not in the list of seeds - Throws Asseertion Error"
    (is (thrown? AssertionError (find-seed \h))))
  (testing "Seed given is in the list of seeds"
    (let [expectedResult "violets"
          param \v]
      (is (= expectedResult (find-seed param))))))
