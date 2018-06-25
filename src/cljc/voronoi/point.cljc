(ns voronoi.point
  (:require [voronoi.util :as u]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defrecord Point [^double x ^double y])

(defn midpoint
  "
  @param {!Point|!voronoi.event.CircleEvent} a
  @param {!Point|!voronoi.event.CircleEvent} b
  @return {!Point}
  "
  ^Point [^Point a ^Point b]
  (let [x (/ (+ (.-x a) (.-x b)) 2)
        y (/ (+ (.-y a) (.-y b)) 2)]
    (->Point x y)))

(defn length
  "
  @param {!Point} p
  "
  ^double [^Point p]
  (u/sqrt (+ (u/sq (.-x p)) (u/sq (.-y p)))))

(defn area2
  "
  @param {!Point|!voronoi.event.CircleEvent} a
  @param {!Point|!voronoi.event.CircleEvent} b
  @param {!Point|!voronoi.event.CircleEvent} c
  @return {!number}
  "
  ^double [a b c]
  (- (* (- (.-x b) (.-x a))
        (- (.-y c) (.-y a)))
     (* (- (.-y b) (.-y a))
        (- (.-x c) (.-x a)))))


(defn ccw
  "
  @param {!Point|!voronoi.event.CircleEvent} a
  @param {!Point|!voronoi.event.CircleEvent} b
  @param {!Point|!voronoi.event.CircleEvent} c
  @return {!number}
  "
  [a b c]
  (let [a (area2 a b c)]
    (cond
      (< a 0.0) -1
      (> a 0.0) 1
      :else   0)))

(defn distance ^double [^Point a ^Point b]
  (u/sqrt (+ (u/sq (- (.-x a) (.-x b)))
             (u/sq (- (.-y a) (.-y b))))))

(defn x-ordered-comparator
  "
  @param {!Point|!voronoi.event.CircleEvent} a
  @param {!Point|!voronoi.event.CircleEvent} b
  @return {!number}
  "
  [^Point a ^Point b]
  (let [c (compare (.-x a) (.-x b))]
    (if (not= c 0)
      c
      (compare (.-y a) (.-y b)))))

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


(defn bound-box [points]
  (let [xs (map :x points)
        ys (map :y points)]
    [(apply min xs)
     (apply max xs)
     (apply min ys)
     (apply max ys)]))

(defn widen-by-percent [[minx maxx miny maxy] percent]
  (let [x-width (- maxx minx)
        y-width (- maxy miny)
        x-add (/ (* x-width (/ percent 100.0)) 2)
        y-add (/ (* y-width (/ percent 100.0)) 2)]
    [(- minx x-add)
     (+ maxx x-add)
     (- miny y-add)
     (+ maxy y-add)]))
