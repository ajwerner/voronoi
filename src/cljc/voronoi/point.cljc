(ns voronoi.point
  (:require [voronoi.util :as u]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord Point [x y])

(defn midpoint [^Point a ^Point b]
  (let [x (/ (+ (:x a) (:x b)) 2)
        y (/ (+ (:y a) (:y b)) 2)]
    (->Point x y)))

(defn length [^Point p]
  (u/sqrt (+ (u/sq (:x p)) (u/sq (:y p)))))

(defn area2 [^Point a  ^Point b  ^Point c]
  (- (* (- (:x b) (:x a))
        (- (:y c) (:y a)))
     (* (- (:y b) (:y a))
        (- (:x c) (:x a)))))

(defn ccw [^Point a  ^Point b  ^Point c]
  (let [a (area2 a b c)]
    (cond
      (< a 0) -1
      (> a 0) 1
      :else   0)))

(defn distance [^Point a ^Point b]
  (u/sqrt (+ (u/sq (- (:x a) (:x b)))
             (u/sq (- (:y a) (:y b))))))

(defn x-ordered-comparator [^Point a ^Point b]
  (let [c (compare (:x a) (:x b))]
    (if (not= c 0)
      c
      (compare (:y a) (:y b)))))

(defn dim-epsilon-comparator [dim]
  (fn [a b]
    (let [ad (dim a)
          bd (dim b)
          veryclose (u/close ad bd)]
      (if veryclose 0 (compare ad bd)))))

(defn dims-epsilon-comparator [d1 d2]
  (let [d1-comp (dim-epsilon-comparator d1)
        d2-comp (dim-epsilon-comparator d2)]
    (fn [a b]
      (let [c (d1-comp a b)]
        (if (not= 0 c) c (d2-comp a b))))))

(def y-ordered-epsilon-comparator
  (dims-epsilon-comparator :y :x))
