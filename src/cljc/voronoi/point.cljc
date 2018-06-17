(ns voronoi.point
  (:require [voronoi.util :refer [Infinity -Infinity sqrt isNaN?]]
            [voronoi.basic-geometry :refer [sq abs close distance]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Point [x y])

(defn midpoint [a b]
  (let [x (/ (+ (:x a) (:x b)) 2)
        y (/ (+ (:y a) (:y b)) 2)]
    (->Point x y)))

(defn length [p]
  (sqrt (+ (sq (:x p)) (sq (:y p)))))

(defn area2 [a b c]
  (- (* (- (:x b) (:x a))
        (- (:y c) (:y a)))
     (* (- (:y b) (:y a))
        (- (:x c) (:x a)))))

(defn ccw [a b c]
  (let [a (area2 a b c)]
    (cond
      (< a 0) -1
      (> a 0) 1
      :else   0)))

(defn x-ordered-comparator [a b]
  (let [c (compare (:x a) (:x b))]
    (if (not= c 0)
      c
      (compare (:y a) (:y b)))))

(defn dim-epsilon-comparator [dim]
  (fn [a b]
    (let [ad (dim a)
          bd (dim b)
          veryclose (close ad bd)]
      (if veryclose 0 (compare ad bd)))))

(defn dims-epsilon-comparator [d1 d2]
  (let [d1-comp (dim-epsilon-comparator d1)
        d2-comp (dim-epsilon-comparator d2)]
    (fn [a b]
      (let [c (d1-comp a b)]
        (if (not= 0 c) c (d2-comp a b))))))

(def y-ordered-epsilon-comparator
  (dims-epsilon-comparator :y :x))
