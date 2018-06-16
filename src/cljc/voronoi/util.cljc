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
