(ns work-with-lists)

(def ^:dynamic *verbose* true)

(defmacro printfv
  [fmt & args]
  `(when *verbose*
     (printf ~fmt ~@args)))

(defmacro with-verbose
  [& body]
  `(binding [*verbose* true] ~@body))


; P01 (*) Find the last box of a list.
(defn my-last
  [items]
  (if (empty? (rest items))
    items
    (recur (rest items))))

; P02 (*) Find the last but one box of a list.
(defn my-but-last
  [items]
  (cond
    (> 2 (count items)) nil
    (= 2 (count items)) items
    (< 2 (count items)) (rest items)))

; P03 (*) Find the K'th element of a list.
(defn element-at
  [items k]
  (cond
    (< k 0) nil
    (= k 0) (first items)
    (> k 0) (recur (rest items) (dec k))))

; P04 (*) Find the number of elements of a list.
(defn find_numbers_element
  [items k]
  (count (filter (fn [x] (= x k)) items)))

; P05 (*) Reverse a list.
(defn reverse_list
  [items]
  (letfn [(concat_rest_list [a b]
            (if
              (empty? a) b
                         (recur (rest a) (cons (first a) b))))]
    (concat_rest_list items '())))

; P06 (*) Find out whether a list is a palindrome.
(defn is_palindrome
  [items]
  (= items (reverse_list items)))

; P07 (**) Flatten a nested list structure.
(defn flatten_list
  [item]
  (letfn [(flatten_list_join [a, b]
            (cond
              (empty? a) b
              (list? (first a)) (concat (flatten_list (first a)) b)
              :else (concat (cons (first a) b) (flatten_list (rest a)))))]
    (flatten_list_join item '())))

; P08 (**) Eliminate consecutive duplicates of list elements.
(defn compress
  [items]
  (letfn [(compress_with_latest [new_items, next_items]
            (cond
              (empty? next_items) new_items
              (= (last new_items) (first next_items)) (recur new_items (rest next_items))
              :else (recur (concat new_items (list (first next_items))) (rest next_items))))]
    (compress_with_latest '() items)))

; (print (compress '(a a a a b c c a a d e e e e)))

; P09 (**) Pack consecutive duplicates of list elements into sublists.
; a is (() () ()) such as, fuck it !!!
(defn my_pack
  [items]
  (letfn [(my_pack' [a, rest_items]
            (printfv "input %s %s\n" a rest_items)

            (cond
              (empty? rest_items) a
              (= (first (last a)) (first rest_items)) (recur (conj (vec (drop-last a)) (conj (last a) (first rest_items))) (rest rest_items))
              :else (recur (conj (vec a) (list (first rest_items))) (rest rest_items))))]
    (my_pack' '() items)))

;(print (my_pack '(a a a a b c c a a d e e e e)))

; P10 (*) Run-length encoding of a list.
(defn my_encode [items]
  (letfn [(count_list [a]
            (list (count a) (first a)))]
    (map count_list (my_pack items))))

;(print (my_encode '(a a a a b c c a a d e e e e)))