(ns clj99.arithmetic-test
  (:require [clj99.arithmetic :refer :all]
            [clojure.test :refer :all]))

(deftest p31
  (testing "P31"
    (is (is-prime 13))
    (is (is-prime 997))
    (is (false? (is-prime 999)))
    (is (false? (is-prime 8911))))) ; Carmichael's number

(deftest p32
  (testing "P32"
    (is (== (gcd 36 63) 9))
    (is (== (gcd 63 36) 9))
    (is (== (gcd 35 64) 1))))

(deftest p33
  (testing "P33"
    (is (coprime? 35 64))
    (is (false? (coprime? 35 65)))))
