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
  "P32 (**) Determine the greatest common divisor of two positive integer
   numbers. Use Euclid's algorithm.
   Example:
     * (gcd 36 63)
     9"
  [x y]
  (loop [a x, b y]
    (if (== b 0)
      a
      (recur b (mod a b)))))


(defn coprime?
  "P33 (*) Determine whether two positive integer numbers are coprime.
   Two numbers are coprime if their greatest common divisor equals 1.
   Example:
     * (coprime? 35 64)
     T"
  [a b]
  (== (gcd a b) 1))


(defn totient-phi
  "P34 (**) Calculate Euler's totient function phi(m).
   Euler's so-called totient function phi(m) is defined as the number of
   positive integers r (1 <= r < m) that are coprime to m.
   Example:
   m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
     * (totient-phi 10)
     4
   Find out what the value of phi(m) is if m is a prime number. Euler's totient
   function plays an important role in one of the most widely used public key
   cryptography methods (RSA). In this exercise you should use the most
   primitive method to calculate this function (there are smarter ways that we
   shall discuss later)."
  [n]
  (loop [i 1, res 0] ; don't know, if this counts as 'the most primitive method'
    (cond (>= i n) res
          (coprime? i n) (recur (inc i) (inc res))
          :else (recur (inc i) res))))


(defn prime-factors
  "P35 (**) Determine the prime factors of a given positive integer.
   Construct a flat list containing the prime factors in ascending order.
   Example:
     * (prime-factors 315)
     (3 3 5 7)"
  [n]
  (loop [num n, fac 2, res []]
    (cond (== num 0) res
          (> (* fac fac) num) (conj res num)
          (== (rem num fac) 0) (recur (quot num fac) fac (conj res fac))
          :else (recur num (inc fac) res))))


;; (defn prime-factors-mult
;;   "P36 (**) Determine the prime factors of a given positive integer (2).
;;    Construct a list containing the prime factors and their multiplicity.
;;    Example:
;;      * (prime-factors-mult 315)
;;      ((3 2) (5 1) (7 1))
;;    Hint: The problem is similar to problem P13."
;;   [n]
;;   (loop [num n, fac 2, set 0, res []]
;;     (print num)
;;     (cond (== num 0) (if (== set 0) res (conj res [fac set]))
;;           (> (* fac fac) num) (conj res [num 1])
;;           (== (rem num fac) 0) (recur (quot num fac) fac (inc set) res)
;;           :else (recur
;;                  num (inc fac) 0 (if (== set 0) res (conj res [fac set]))))))
