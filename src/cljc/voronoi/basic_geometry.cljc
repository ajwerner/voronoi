(ns voronoi.basic-geometry
  (:require [voronoi.util :refer [sqrt]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Geometry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (defn sq [x] (* x x))

(defn abs [n] (max n (- n)))

(defn within-epsilon [a b epsilon]
  (< (abs (- a b)) epsilon))

(defn distance [a b]
  (let [[{x1 :x y1 :y} {x2 :x y2 :y}] [a b]
        dist (sqrt (+ (sq (- x1 x2))
                      (sq (- y1 y2))))]
    dist))
(def epsilon 1e-8)

(defn close [a b]
  (within-epsilon a b epsilon))
