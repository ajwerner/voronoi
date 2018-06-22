(ns voronoi.util)

(defonce PI
  #?(:clj java.lang.Math/PI
     :cljs js/Math.PI))

(defonce Infinity
  #?(:clj java.lang.Double/POSITIVE_INFINITY
     :cljs js/Number.POSITIVE_INFINITY))

(defonce -Infinity
  #?(:clj java.lang.Double/NEGATIVE_INFINITY
     :cljs js/Number.NEGATIVE_INFINITY))

(defn isNaN? [^double n]
  #?(:clj (java.lang.Double/isNaN n)
     :cljs (js/isNaN n)))

(defn sqrt ^double [^double v]
  #?(:clj (java.lang.Math/sqrt v)
     :cljs (.sqrt js/Math v)))

(defn sin ^double [^double v]
     #?(:clj (java.lang.Math/sin v)
        :cljs (js/Math.sin v)))

(defn cos [v]
     #?(:clj (java.lang.Math/cos v)
        :cljs (js/Math.cos v)))

(defn random []
     #?(:clj (java.lang.Math/random)
        :cljs (js/Math.random)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Geometry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sq ^double [^double x] (* x x))

(defn abs ^double [n] (max n (- n)))

(defn within-epsilon [^double a ^double b ^double epsilon]
  (< (abs (- a b)) epsilon))

(def epsilon 1e-8)

(defn close [^double a ^double b]
  (within-epsilon a b epsilon))
