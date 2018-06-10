(ns voronoi.util)

(def Infinity js/Number.POSITIVE_INFINITY)
(def -Infinity js/Number.NEGATIVE_INFINITY)
(defn isNaN? [n]
  (js/Number.isNaN n))
