(ns voronoi.util)

(def Infinity
  #?(:clj java.lang.Double/POSITIVE_INFINITY
     :cljs js/Number.POSITIVE_INFINITY))

(def -Infinity
  #?(:clj java.lang.Double/NEGATIVE_INFINITY
     :cljs js/Number.NEGATIVE_INFINITY))

(defn isNaN? [n]
  #?(:clj (java.lang.Double/isNaN n)
     :cljs (js/isNaN n)))

(defn sqrt [v]
  #?(:clj (java.lang.Math/sqrt v)
     :cljs (.sqrt js/Math v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Geometry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sq [x] (* x x))

(defn abs [n] (max n (- n)))

(defn within-epsilon [a b epsilon]
  (< (abs (- a b)) epsilon))


(def epsilon 1e-8)

(defn close [a b]
  (within-epsilon a b epsilon))
