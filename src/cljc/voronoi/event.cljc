(ns voronoi.event
  (:require [voronoi.arc :as arc]
            [voronoi.point :as point]
            [voronoi.util :as u]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord CircleEvent [x y vert arc])

(defn is-circle [ev] (instance? CircleEvent ev))

(defn event-comparator [a b]
  (let [ay (:y a)
        by (:y b)
        c (if-not (u/close ay by)
            (compare ay by))]
    (if (and (some? c) (not= c 0))
      c
      (let [aCircle (is-circle a)
            bCircle (is-circle b)
            cx (compare (:x a) (:x b))
            breaker (cond
                      (not (or aCircle bCircle)) cx
                      (and aCircle (not bCircle)) -1
                      (and bCircle (not aCircle)) 1
                      :else (arc/arc-comparator (:arc a) (:arc b)))]
        breaker))))
