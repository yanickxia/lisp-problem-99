(ns work-with-lists)

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
