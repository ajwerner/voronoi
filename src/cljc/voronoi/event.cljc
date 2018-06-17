(ns voronoi.event
  (:require [voronoi.arc :as arc]
            [voronoi.point :as point]
            [voronoi.util :as u]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord CircleEvent [x y vert arc])

(defn is-circle [ev] (instance? CircleEvent ev))

(defn comparator [a b]
  (let [ay (:y a)
        by (:y b)
        c (if-not (u/close ay by)
            (compare ay by))]
    (if (and (some? c) (not= c 0))
      c
      (let [aCircle (is-circle a)
            bCircle (is-circle b)
            aCcw (point/ccw a (:vert a) (:point (:arc a)))
            bCcw (point/ccw b (:vert b) (:point (:arc b)))
            oCcw (point/ccw (:point (:arc a)) (:vert a) (:point (:arc b)))
            cx (compare (:x a) (:x b))
            weVert (if (and aCircle bCircle)
                     (u/close (:x (:vert a)) (:x (:vert b))))
            breaker (cond
                      (not (or aCircle bCircle)) cx
                      (and aCircle (not bCircle)) -1
                      (and bCircle (not aCircle)) 1
                      :else (arc/comparator (:arc a) (:arc b)))]
        breaker))))