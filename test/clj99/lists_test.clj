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

(deftest p05
  (testing "P05"
    (is (= (mreverse '(a b c)) '(c b a)))))

(deftest p06
  (testing "P05"
    (is (= (palindrome? '(x a m a x)) true))
    (is (= (palindrome? '()) true))
    (is (= (palindrome? '(a b c)) false))))

(deftest p07
  (testing "P07"
    (is (= (mflatten '(a (b (c d) e))) '(a b c d e)))))

(deftest p08
  (testing "P08"
    (is (= (clj99.lists/compress '(a a a a b c c a a d e e e e))
           '(a b c a d e)))))
