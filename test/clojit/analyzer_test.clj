(ns clojit.analyzer-test
  (:refer-clojure :exclude [macroexpand-1])
  (:require [clojure.test :refer :all]
            [clojit.analyzer :refer :all]))

(deftest ast-table-number-tests

  (testing "Number Tests"
    (is (number-const? (ast 1.2)))
    (is (number-const? (ast 1))))
    (is (not (number-const? (ast "test"))))

  (testing "Float Int"
    (is (is-int? (ast 1)))
    (is (not (is-int? (ast 1.2))))))

(deftest ast-table-string-tests

  (testing "Number Tests"
    (is (string-const? (ast "test")))
    (is (not (string-const? (ast 1))))))


(deftest ast-table-keyword-tests

  (testing "Number Tests"
    (is (keyword-const? (ast :test)))
    (is (not (keyword-const? (ast 1))))))
