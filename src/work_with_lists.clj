(ns work-with-lists)

(def ^:dynamic *verbose* true)

(defn conj*
  [s x]
  (conj (vec s) x))

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



; P11 (*) Modified run-length encoding
(defn encode-modified [item]
  (letfn [(encode-list [a]
            (if (= (first a) 1)
              (second a)
              a))]
    (map encode-list (my_encode item))))

(assert (= (encode-modified '(a a a a b c c a a d e e e e)) '((4 a) b (2 c) (2 a) d (4 e))))

; P12 (**) Decode a run-length encoded list.
(defn decode-encode-list [item]
  (letfn [(expand' [x]
            (if (list? x)
              (repeat (first x) (second x))
              (identity x)))]
    (flatten (map expand' item))))

(assert (= (decode-encode-list '((4 a) b (2 c) (2 a) d (4 e))) '(a a a a b c c a a d e e e e)))

; P13 (**) Run-length encoding of a list (direct solution)
(defn encode-direct [item]
  (cond
    (= 1 (count item)) item
    (list? (first item)) (if (= (second (first item)) (second item))
                           (encode-direct (cons (list (+ 1 (first (first item))) (second item)) (rest (rest item))))
                           (cons (first item) (encode-direct (rest item))))
    :else (if (= (first item) (second item))
            (encode-direct (cons (list 1 (first item)) (rest item)))
            (cons (first item) (encode-direct (rest item))))))

(assert (= (encode-direct '(a a a a b c c a a d e e e e)) '((4 a) b (2 c) (2 a) d (4 e))))

; P14 (*) Duplicate the elements of a list.
(defn dupli [item]
  (if (empty? item)
    item
    (concat (repeat 2 (first item)) (dupli (rest item)))))

(assert (= (dupli '(a b c c d)) '(a a b b c c c c d d)))


; P15 (**) Replicate the elements of a list a given number of times.
(defn repli [item times]
  (flatten (map (partial repeat times) item)))

(assert (= (repli '(a b c) 3) '(a a a b b b c c c)))

; P16 (**) Drop every N'th element from a list.
(defn drop-every-n [items nth]
  (if (empty? items)
    items
    (concat (take (- nth 1) items) (drop-every-n (drop nth items) nth))))

(assert (= (drop-every-n '(a b c d e f g h i k) 3) '(a b d e g h k)))

; P17 (*) Split a list into two parts; the length of the first part is given.

(defn split' [items nth]
  (list (take nth items) (drop nth items)))
(assert (= (split' '(a b c d e f g h i k) 3) '((a b c) (d e f g h i k))))

; P18 (**) Extract a slice from a list.
(defn slice' [item start end]
  (drop (- start 1) (take end item)))
(assert (= (slice' '(a b c d e f g h i k) 3 7) '(c d e f g)))

;  P19 (**) Rotate a list N places to the left.
(defn rotate' [item nth]
  (cond
    (= 0 nth) item
    (> nth 0) (rotate' (conj* (rest item) (first item)) (dec nth))
    (< nth 0) (rotate' (cons (last item) (drop-last item)) (inc nth))
    ))

(assert (= (rotate' '[a b c d e f g h] 3) '[d e f g h a b c]))
(assert (= (rotate' '[a b c d e f g h] -2) '[g h a b c d e f]))

; P20 (*) Remove the K'th element from a list.
; 没考虑负数的at
(defn remove-at' [item at]
  (concat (take (- at 1) item) (drop at item)))

(assert (= (remove-at' '(a b c d) 2) '(a c d)))


























