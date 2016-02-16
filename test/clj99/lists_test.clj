(ns clj99.lists-test
  (:require  [clojure.test :refer :all]
             [clj99.lists :refer :all]))

(deftest p01
  (testing "PO1"
    (is (= (mlast '(a b c d)) '(d)))))

(deftest p02
  (testing "P02"
    (is (= (mbutlast '(a b c d)) '(c d)))))

(deftest p03
  (testing "P03"
    (is (= (elem-at '(a b c d e) 3) 'c))))

(deftest p04
  (testing "P04"
    (is (= (mlen '(a b c d e)) 5))))
