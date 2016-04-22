(ns clj99.arithmetic
  "Problems from 31 through 41. Arithmetic operations."
  (:require [clojure.math.numeric-tower :as m]))


(defn is-prime
  "P31 (**) Determine whether a given integer number is prime.
   Example:
     * (is-prime 7)
     T"
  [n]
  (if (even? n)
    false
    (letfn [(%witness [times]
              (if (== times 0)
                true
                (let [a (inc (rand-int (dec n)))
                      [r s] (loop [r 1, s (quot (dec n) 2)]
                              (if (even? s)
                                (recur (inc r) (quot s 2))
                                [r s]))
                      t1 (= (int (mod (m/expt a s) n)) 1)
                      t2 (loop [j 0]
                           (cond (> j (dec r)) false
                                 (let [p (mod (m/expt a (* (m/expt 2 j) s)) n)]
                                   (= (int p) (dec n)))
                                 true
                                 :else (recur (inc j))))]
                  (and (or t1 t2) (%witness (dec times))))))]
      (%witness 10))))


(defn gcd
  "P32 (**) Determine the greatest common divisor of two positive integer numbers.
   Use Euclid's algorithm.
   Example:
     * (gcd 36 63)
     9"
  [x y]
  (loop [a x, b y]
    (if (== b 0)
      a
      (recur b (mod a b)))))


(defn coprime? [a b]
  "P33 (*) Determine whether two positive integer numbers are coprime.
   Two numbers are coprime if their greatest common divisor equals 1.
   Example:
     * (coprime? 35 64)
     T"
  (== (gcd a b) 1))
