(ns arithmetic
  (:require [work-with-lists :as wwl]))

; P31 (**) Determine whether a given integer number is prime.
(defn is-prime [n]
  (letfn [(is-prime' [x]
            (cond
              (= x n) true
              (= 0 (mod n x)) false
              :else (is-prime' (inc x))))]
    (is-prime' 2)))

(assert (= (is-prime 13) true))

; P32 (**) Determine the greatest common divisor of two positive integer numbers.
(defn gcd [x y]
  (if (= y 0) x
              (gcd y (mod x y))))

(assert (= (gcd 36 63) 9))


; P33 (*) Determine whether two positive integer numbers are coprime.
(defn coprime [x y]
  (= 1 (gcd x y)))

(assert (= (coprime 35 64) true))


; P34 (**) Calculate Euler's totient function phi(m).
(defn totient-phi [m]
  (cond
    (= 1 m) 1
    (= 2 m) 1
    (= (- m 2) (count (filter (partial coprime m) (range 2 m)))) (+ 1 (totient-phi (dec m)))
    :else (totient-phi (dec m))))

(assert (= (totient-phi 10) 4))



; P35 (**) Determine the prime factors of a given positive integer.
(defn prime-factors [m]
  (letfn [(prime-factors' [n]
            (cond
              (= m n) (list m)
              (is-prime n) (if (= (mod m n) 0)
                             (conj (prime-factors (/ m n)) n)
                             (prime-factors' (inc n)))
              :else (prime-factors' (inc n)))
            )]
    (prime-factors' 2)))

(assert (= (prime-factors 315) '(3 3 5 7)))


; P36 (**) Determine the prime factors of a given positive integer (2).
(defn prime-factors-mult [m]
  (wwl/my_encode (prime-factors m)))

(assert (= (prime-factors-mult 315) '((2 3) (1 5) (1 7))))

; P37 (**) Calculate Euler's totient function phi(m) (improved).

(defn phi-improved [m]
  (let [m-prime-factors-mult (prime-factors-mult m)]
    (.toBigInteger (* m (reduce * (map (fn [x] (- 1 (/ 1 (second x)))) m-prime-factors-mult))))))

; (Math/pow (* (second x) (- (second x) 1)) (- (first x) 1))
; (fn [x] (* (- (second x) 1) (Math/pow (second x) (- (first x) 1))))

(assert (= (phi-improved 12) 4))

; P38 (*) Compare the two methods of calculating Euler's totient function.

;P39 (*) A list of prime numbers.
(defn list-prime [lower upper]
  (filter is-prime (range lower upper)))
(assert (= (list-prime 2 10) '(2 3 5 7)))


(defn is-goldbach [m]
  (let [prime-list (list-prime 2 m)]
    (not (empty? (filter (fn [x] (is-prime (- m x))) prime-list)))
    ))


; P40 (**) Goldbach's conjecture.
(defn goldbach [m]
  (let [prime-list (list-prime 2 m)]
    (let [x (first (filter (fn [x] (is-prime (- m x))) prime-list))]
      (list x (- m x)))))

(assert (= (goldbach 28) '(5 23)))

; P41 (**) A list of Goldbach compositions.
(defn goldbach-list [lower upper]
  (map goldbach (filter is-goldbach (range lower upper))))
