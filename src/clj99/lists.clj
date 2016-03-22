(ns clj99.lists
  "Problems from 1 through 28. Working with lists.")


(defn mlast
  "P01 (*) Find the last box of a list.
   Example:
     * (my-last '(a b c d))
     (D)"
  [sq]
  (let [nx (next sq)]
    (if nx
      (recur nx)
      sq)))


(defn mbutlast
  "P02 (*) Find the last but one box of a list.
   Example:
     * (my-but-last '(a b c d))
     (C D)"
  [sq]
  (if (nnext sq)
    (recur (rest sq))
    sq))


(defn elem-at
  "P03 (*) Find the K'th element of a list.
   The first element in the list is number 1.
   Example:
     * (element-at '(a b c d e) 3)
     C"
  [sq k]
  (loop [s sq, n 1]
    (if (= n k)
      (first s)
      (recur (rest s) (inc n)))))


(defn mlen
  "P04 (*) Find the number of elements of a list."
  [sq]
  (loop [s sq, ln 0]
    (if (seq s)
      (recur (rest s) (inc ln))
      ln)))


(defn mreverse
  "P05 (*) Reverse a list."
  [lst]
  (loop [l lst, res nil]
    (if (seq l)
      (recur (rest l) (concat (list (first l)) res))
      res)))


(defn palindrome?
  "P06 (*) Find out whether a list is a palindrome.
   A palindrome can be read forward or backward; e.g. (x a m a x)."
  [lst]
  (every? identity (map = lst (reverse lst))))


(defn mflatten
  "P07 (**) Flatten a nested list structure.
   Transform a list, possibly holding lists as elements into a `flat' list by
   replacing each list with its elements (recursively).
   Hint: Use the predefined functions list and append.
   Example:
     * (my-flatten '(a (b (c d) e)))
     (A B C D E)"
  [tree]
  (letfn [(mloop% [t res]
            (if (seq t)
              (if (seq? (first t))
                (recur (rest t) (concat (mloop% (first t) nil) res))
                (recur (rest t) (cons (first t) res)))
              res))]
    (reverse (mloop% tree nil))))


(defn compress
  "P08 (**) Eliminate consecutive duplicates of list elements.
   If a list contains repeated elements they should be replaced with a single
   copy of the element. The order of the elements should not be changed.
   Example:
     * (compress '(a a a a b c c a a d e e e e))
     (A B C A D E)"
  [lst]
  (loop [c (first lst), l lst, res (and c (list c))]
    (if (seq l)
      (if (= (first l) c)
        (recur c (rest l) res)
        (recur (first l) (rest l) (cons (first l) res)))
      (reverse res))))


(defn pack
  "P09 (**) Pack consecutive duplicates of list elements into sublists.
   If a list contains repeated elements they should be placed in
   separate sublists.
   Example:
     * (pack '(a a a a b c c a a d e e e e))
     ((A A A A) (B) (C C) (A A) (D) (E E E E))"
  [lst]
  (loop [c (first lst), l lst, res nil, sub nil]
    (if (seq l)
      (if (= (first l) c)
        (recur c (rest l) res (cons c sub))
        (recur (first l) (rest l) (cons sub res) (list (first l))))
      (reverse (cons sub res)))))


(defn encode
  "P10 (*) Run-length encoding of a list.
   Use the result of problem P09 to implement the so-called run-length
   encoding data compression method. Consecutive duplicates of elements
   are encoded as lists (N E) where N is the number of duplicates
   of the element E.
   Example:
     * (encode '(a a a a b c c a a d e e e e))
     ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))"
  [lst]
  (loop [c (first lst), l lst, res nil, n 0]
    (if (seq l)
      (if (= (first l) c)
        (recur c (rest l) res (inc n))
        (recur (first l) (rest l) (cons (list n c) res) 1))
      (reverse (cons (list n c) res)))))


(defn encode-modified
  "P11 (*) Modified run-length encoding.
   Modify the result of problem P10 in such a way that if an element
   has no duplicates it is simply copied into the result list. Only
   elements with duplicates are transferred as (N E) lists.
   Example:
     * (encode-modified '(a a a a b c c a a d e e e e))
     ((4 A) B (2 C) (2 A) D (4 E))"
  [lst]
  (loop [c (first lst), l lst, res nil, n 0]
    (if (seq l)
      (if (= c (first l))
        (recur c (rest l) res (inc n))
        (recur (first l) (rest l) (cons (if (= n 1) c (list n c)) res) 1))
      (reverse (cons (if (= n 1) c (list n c)) res)))))


(defn decode-rl
  "P12 (**) Decode a run-length encoded list.
   Given a run-length code list generated as specified in problem P11.
   Construct its uncompressed version."
  [encoded-lst]
  (->> encoded-lst
       (map #(if (seq? %) (repeat (first %) (second %)) (list %)))
       (reduce concat)))


(defn encode-direct
  "P13 (**) Run-length encoding of a list (direct solution).
   Implement the so-called run-length encoding data compression method directly.
   I.e. don't explicitly create the sublists containing the duplicates, as in
   problem P09, but only count them. As in problem P11, simplify the result list
   by replacing the singleton lists (1 X) by X.
   Example:
     * (encode-direct '(a a a a b c c a a d e e e e))
     ((4 A) B (2 C) (2 A) D (4 E))
   ALREADY DONE IN ENCODE-MODIFIED"
  [lst]
  (encode-modified lst))


(defn dupli
  "P14 (*) Duplicate the elements of a list.
   Example:
     * (dupli '(a b c c d))
     (A A B B C C C C D D)"
  [lst]
  (transduce (map #(list % %)) concat lst))


(defn repli
  "P15 (**) Replicate the elements of a list a given number of times.
   Example:
     * (repli '(a b c) 3)
     (A A A B B B C C C)"
  [lst n]
  (reduce concat (map #(repeat n %) lst)))


(defn mdrop 
  "P16 (**) Drop every N'th element from a list.
   Example:
     * (drop '(a b c d e f g h i k) 3)
     (A B D E G H K)"
  [lst n]
  (loop [l lst, c 1, res nil]
    (if (seq l)
      (if (= c n)
        (recur (rest l) 1 res)
        (recur (rest l) (inc c) (cons (first l) res)))
      (reverse res))))


(defn msplit
  "P17 (*) Split a list into two parts; the length of the first part is given.
   Do not use any predefined predicates.
   Example:
     * (split '(a b c d e f g h i k) 3)
     ((A B C) (D E F G H I K))"
  [lst n]
  (loop [l lst, c 0, part nil]
    (if (seq l)
      (if (= c n)
        (list (reverse part) l)
        (recur (rest l) (inc c) (cons (first l) part)))
      (list (reverse part) nil))))


(defn slice
  "P18 (**) Extract a slice from a list.
   Given two indices, I and K, the slice is the list containing the
   elements between the I'th and K'th element of the original list
   (both limits included). Start counting the elements with 1.
   Example:
     * (slice '(a b c d e f g h i k) 3 7)
     (C D E F G)"
  [lst s e]
  (loop [l lst, c 1, res nil]
    (cond (or (> c e) (not (seq l))) (reverse res)
          (and (>= c s) (<= c e)) (recur (rest l) (inc c) (cons (first l) res))
          :else (recur (rest l) (inc c) nil))))


(defn rotate
  "P19 (**) Rotate a list N places to the left.
   Examples:
     * (rotate '(a b c d e f g h) 3)
     (D E F G H A B C)
     * (rotate '(a b c d e f g h) -2)
     (G H A B C D E F)
   Hint: Use the predefined functions length and append, as well as
         the result of problem P17."
  [lst n]
  (let [[pref postf] (split-at (mod n (count lst)) lst)]
    (concat postf pref)))


(defn remove-at 
  "P20 (*) Remove the K'th element from a list.
   Example:
     * (remove-at '(a b c d) 2)
     (A C D)"
  [lst k]
  (cond (not (seq lst)) nil
        (= k 1) (rest lst)
        :else (cons (first lst) (remove-at (rest lst) (dec k)))))


(defn insert-at 
  "P21 (*) Insert an element at a given position into a list.
   Example:
     * (insert-at 'alfa '(a b c d) 2)
     (A ALFA B C D)"
  [elem lst k]
  (cond (not (seq lst)) (list elem)
        (= k 1) (cons elem lst)
        :else (cons (first lst) (insert-at elem (rest lst) (dec k)))))


(defn mrange
  "P22 (*) Create a list containing all integers within a given range.
   If first argument is smaller than second, produce a list in decreasing order.
   Example:
     * (range 4 9)
     (4 5 6 7 8 9)"
  [a b]
  (take (inc (Math/abs (- a b))) (iterate (if (< a b) inc dec) a)))


(defn rnd-select
  "P23 (**) Extract a given number of randomly selected elements from a list.
   The selected items shall be returned in a list.
   Example:
     * (rnd-select '(a b c d e f g h) 3)
     (E D A)
   Hint: Use the built-in random number generator and the result
         of problem P20."
  [lst k]
  (loop [l lst, n k, res nil]
    (if (or (== n 0) (not (seq l)))
      res
      (let [pos (inc (rand-int (count l)))
            elem (elem-at l pos)]
        (recur (remove-at l pos) (dec n) (cons elem res))))))


(defn lotto-select
  "P24 (*) Lotto: Draw N different random numbers from the set 1..M.
   The selected numbers shall be returned in a list.
   Example:
     * (lotto-select 6 49)
     (23 1 17 33 21 37)
   Hint: Combine the solutions of problems P22 and P23."
  [k m]
  (rnd-select (mrange 1 m) k))


(defn rnd-permu
  "P25 (*) Generate a random permutation of the elements of a list.
   Example:
     * (rnd-permu '(a b c d e f))
     (B A D C E F)
   Hint: Use the solution of problem P23."
  [lst]
  (rnd-select lst (count lst)))


(defn combination
  "P26 (**) Generate the combinations of K distinct objects chosen from
   the N elements of a list. In how many ways can a committee of 3 be chosen
   from a group of 12 people? We all know that there
   are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial
   coefficients). For pure mathematicians, this result may be great. But we want
   to really generate all the possibilities in a list.
   Example:
     * (combination 3 '(a b c d e f))
     ((A B C) (A B D) (A B E) ... ) "
  [n lst]
  )
