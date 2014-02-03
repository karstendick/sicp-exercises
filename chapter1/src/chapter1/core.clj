(ns chapter1.core)

(defn exercise-1-1
  []
  ;;EXERCISE 1.1
  10
  ;; 10

  (+ 5 3 4)
  ;; 12

  (- 9 1)
  ;; 8

  (/ 6 2)
  ;; 3

  (+ (* 2 4) (- 4 6))
  ;; 6

  (def a 3)

  (def b (+ a 1))

  (+ a b (* a b))
  ;; 19

  (= a b)
  ;; false

  (if (and (> b a) (< b (* a b)))
    b
    a)
  ;; 4

  (cond (= a 4) 6
        (= b 4) (+ 6 7 a)
        :else 25)
  ;; 16

  (+ 2 (if (> b a) b a))
  ;; 6

  (* (cond (> a b) a
           (< a b) b
           :else -1)
     (+ a 1))
  ;; 16
  )

(defn exercise-1-2
  []
  (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
     (* 3 (- 6 2) (- 2 7))))

(defn exercise-1-3
  [a b c]
  (defn max-2-of-3
    []
    (cond (and (< a b) (< a c)) [b c]
          (and (< b a) (< b c)) [a c]
          :else [a b]))
  (let [[x y] (max-2-of-3)]
    (+ (* x x) (* y y))))


;;EXERCISE 1.4
(defn a-plus-abs-b
  "a+b when b > 0; otherwise, a-b"
  [a b]
  ((if (> b 0) + -) a b))


(defn square
  [x]
  (* x x))

(defn abs
  [x]
  (if (< x 0)
    (- x)
    x))

(defn good-enough?
  [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn average
  [x y]
  (/ (+ x y) 2))

(defn improve
  [guess x]
  (average guess (/ x guess)))

(defn sqrt-iter
  [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(defn sqrt
  [x]
  (sqrt-iter 1.0 x))


;;EXERCISE 1.6
(defn new-if
  [predicate then-clause else-clause]
  (cond predicate then-clause
        :else else-clause))

(new-if (= 2 3) 0 5)
;; 5

(new-if (= 1 1) 0 5)
;; 0

(defn sqrt-iter
  [guess x]
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
;; StackOverflowError


;; EXERCISE 1.7
(defn sqrt-iter
  [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(sqrt 0.0001)
;; 0.03230844...

(defn good-enough?
  [guess x last-guess]
  (< (abs (- guess last-guess)) 0.001))

(defn average
  [x y]
  (/ (+ x y) 2))

(defn improve
  [guess x]
  (average guess (/ x guess)))

(defn sqrt-iter
  [guess x last-guess]
  (if (good-enough? guess x last-guess)
    guess
    (sqrt-iter (improve guess x)
               x
               guess)))

(defn sqrt
  [x]
  (sqrt-iter 1.0 x x))

(sqrt 0.0001)
;; 0.010000...


;; EXERCISE 1.8

(defn improve
  [guess x]
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(defn cube-root-iter
  [guess x last-guess]
  (if (good-enough? guess x last-guess)
    guess
    (cube-root-iter (improve guess x)
                    x
                    guess)))

(defn cube-root
  [x]
  (cube-root-iter 1.0 x x))


;; EXERCISE 1.9
(defn plus1
  [a b]
  (if (= a 0)
    b
    (inc (plus1 (dec a) b))))

;; (plus1 4 5)
;; (inc (plus1 3 5))
;; (inc (inc (plus1 2 5)))
;; (inc (inc (inc (plus1 1 5))))
;; (inc (inc (inc (inc (plus1 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9

(defn plus2
  [a b]
  (if (= a 0)
    b
    (plus2 (dec a) (inc b))))

;; (plus2 4 5)
;; (plus2 3 6)
;; (plus2 2 7)
;; (plus2 1 8)
;; (plus2 0 9)
;; 9

;;EXERCISE 1.10
(defn A
  [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (A (- x 1)
                 (A x (- y 1)))))

(A 1 10)
;; 1024

(A 2 4)
;; 65536

(A 3 3)
;; 65536

(defn f [n] (A 0 n))
;; 2*n

(defn g [n] (A 1 n))
;; 2^n

(defn h [n] (A 2 n))
;; 2^2^... (n times)

(defn k [n] (* 5 n n))
;; 5*n^2



;; EXERCISE 1.11

(defn rf
  [n]
  (cond (< n 3) n
        :else (+ (rf (dec n))
                 (* 2 (rf (- n 2)))
                 (* 3 (rf (- n 3))))))



(defn f-iter
  [x y z c]
  (if (zero? c)
    x
    (f-iter y
            z
            (+ (* 3 x) (* 2 y) z)
            (dec c))))

(defn f
  [n]
  (f-iter 0 1 2 n))


;; EXERCISE 1.12

(defn pascal
  "Computes numbers from Pascal's triangle.
  row and col are the 1-based row and column of an entry"
  [row col]
  (cond (= 1 col) 1
        (= row col) 1
        :else (+ (pascal (dec row) (dec col))
                 (pascal (dec row) col))))


;; EXERCISE 1.16

;; Logarithmic iteration
(defn fast-expt
  [b n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt b (/ n 2)))
        :else (* b (fast-expt b (- n 1)))))


(defn exp-iter
  [a b n]
  (cond (zero? n) a
        (even? n) (exp-iter a (square b) (/ n 2))
        :else (exp-iter (* a b) b (dec n))))

(defn exp
  [b n]
  (exp-iter 1 b n))


;;EXERCISE 1.17
(defn mult
  [a b]
  (if (= b 0)
    0
    (+ a (mult a (- b 1)))))

(defn times2 [x] (+ x x))
(defn halve
  [x]
  (if (even? x) (/ x 2)
    (throw (Exception. "Invalid argument: halve accepts only even numbers"))))

(defn mult-fast
  [a b]
  (cond (zero? b) 0
        (even? b) (times2 (mult-fast a (halve b)))
        :else (+ a (mult-fast a (dec b)))))

;;EXERCISE 1.18
(defn mult-iter
  [x y z]
  (cond (zero? y) z
        (even? y) (mult-iter (times2 x) (halve y) z)
        :else (mult-iter x (dec y) (+ x z))))

(defn mult
  [x y]
  (mult-iter x y 0))


;;EXERCISE 1.19

(defn fib-iter
  [a b p q c]
  (cond (= c 0) b
        (even? c)
        (fib-iter a
                  b
                  (+ (* p p) (* q q)) ; compute p'
                  (+ (* q q) (* 2 p q)) ; compute q'
                  (/ c 2))
        :else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- c 1))))

(defn fib
  [n]
  (fib-iter 1 0 0 1 n))
















