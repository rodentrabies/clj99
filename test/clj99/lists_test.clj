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

(deftest p09
  (testing "P09"
    (is (= (clj99.lists/pack '(a a a a b c c a a d e e e e))
           '((a a a a) (b) (c c) (a a) (d) (e e e e))))))

(deftest p10
  (testing "P10"
    (is (= (clj99.lists/encode '(a a a a b c c a a d e e e e))
           '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e))))))

(deftest p11
  (testing "P11"
    (is (= (clj99.lists/encode-modified '(a a a a b c c a a d e e e e))
           '((4 a) b (2 c) (2 a) d (4 e))))))

(deftest p12
  (testing "P12"
    (is (= (clj99.lists/decode-rl '((4 a) b (2 c) (2 a) d (4 e)))
           '(a a a a b c c a a d e e e e)))))

(deftest p13
  (testing "P13"
    (is (= (clj99.lists/encode-direct '(a a a a b c c a a d e e e e))
           '((4 a) b (2 c) (2 a) d (4 e))))))

(deftest p14
  (testing "P14"
    (is (= (clj99.lists/dupli '(a b c c d))
           '(a a b b c c c c d d)))))

(deftest p15
  (testing "P15"
    (is (= (clj99.lists/repli '(a b c) 3)
           '(a a a b b b c c c)))))

(deftest p16
  (testing "P16"
    (is (= (clj99.lists/mdrop '(a b c d e f g h i k) 3)
           '(a b d e g h k)))))

(deftest p17
  (testing "P17"
    (is (= (clj99.lists/msplit '(a b c d e f g h i k) 3)
           '((a b c) (d e f g h i k))))))

(deftest p18
  (testing "P18"
    (is (= (clj99.lists/slice '(a b c d e f g h i k) 3 7)
           '(c d e f g)))))

(deftest p19
  (testing "P19"
    (is (= (clj99.lists/rotate '(a b c d e f g h) 3)
           '(d e f g h a b c)))
    (is (= (clj99.lists/rotate '(a b c d e f g h) -2)
           '(g h a b c d e f)))))

(deftest p20
  (testing "P20"
    (is (= (clj99.lists/remove-at '(a b c d) 2) '(a c d)))))

(deftest p21
  (testing "P21"
    (is (= (clj99.lists/insert-at 'alfa '(a b c d) 2) '(a alfa b c d)))))

(deftest p22
  (testing "P22"
    (is (= (clj99.lists/mrange 4 9) '(4 5 6 7 8 9)))
    (is (= (clj99.lists/mrange 9 4) '(9 8 7 6 5 4)))))

(deftest p23
  (let [testval '(a b c d e f g h)]
   (testing "P23"
     (dotimes [n 10]
       (let [k (rand-int (count testval)), v (clj99.lists/rnd-select testval k)]
        (is (and (= (count v) k)) (every? (into #{} testval) v)))))))
