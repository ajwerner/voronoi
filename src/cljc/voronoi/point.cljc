(ns voronoi.point
  (:require [voronoi.util :as u]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Point [x y])

(defn midpoint [a b]
  (let [x (/ (+ (:x a) (:x b)) 2)
        y (/ (+ (:y a) (:y b)) 2)]
    (->Point x y)))

(defn length [p]
  (u/sqrt (+ (u/sq (:x p)) (u/sq (:y p)))))

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

(defn distance [a b]
  (let [[{x1 :x y1 :y} {x2 :x y2 :y}] [a b]
        dist (u/sqrt (+ (u/sq (- x1 x2))
                        (u/sq (- y1 y2))))]
    dist))

(defn x-ordered-comparator [a b]
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
