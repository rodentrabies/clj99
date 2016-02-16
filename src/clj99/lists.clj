(ns clj99.lists
  "Problems from 1 through 28. Working with lists.")


(defn mlast
  "P01 (*) Find the last box of a list.
   Example:
     * (my-last '(a b c d))
     (D)
  "
  [sq]
  (let [nx (next sq)]
    (if nx
      (recur nx)
      sq)))


(defn mbutlast
  "P02 (*) Find the last but one box of a list.
   Example:
     * (my-but-last '(a b c d))
     (C D)
  "
  [sq]
  (if (nnext sq)
    (recur (rest sq))
    sq))


(defn elem-at
  "P03 (*) Find the K'th element of a list.
   The first element in the list is number 1.
   Example:
     * (element-at '(a b c d e) 3)
     C
  "
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
     (A B C D E)
  "
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
     (A B C A D E)
  "
  [lst]
  (loop [c (first lst), l lst, res (and c (list c))]
    (if (seq l)
      (if (= (first l) c)
        (recur c (rest l) res)
        (recur (first l) (rest l) (cons (first l) res)))
      (reverse res))))
