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


















