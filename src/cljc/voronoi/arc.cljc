(ns voronoi.arc
  (:require [voronoi.util :refer [Infinity -Infinity close]]
            [voronoi.point :refer [->Point map->Point
                                   y-ordered-epsilon-comparator
                                   x-ordered-comparator
                                   ccw midpoint]]
            [voronoi.edge :refer [intersect-edges]]
            [voronoi.break-point :as bp :refer [break-point-point]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arcs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ArcComparable
  (left-x [this sweep-y])
  (left-y [this sweep-y x])
  (right-x [this sweep-y])
  (right-y [this sweep-y x])
  (left-bound [this sweep-y])
  (right-bound [this sweep-y]))

(def nil-left-bound-point
  (->Point -Infinity -Infinity))

(def nil-right-bound-point
  (->Point Infinity -Infinity))

;; TODO revisit the nil points based on the slope of the line

(defrecord Arc [point left right added-at]
  ArcComparable
  (left-x [this sweep-y]
    (if (nil? left) -Infinity (bp/point-x (:pp left) sweep-y)))
  (right-x [this sweep-y]
    (if (nil? right) Infinity (bp/point-x (:pp right) sweep-y)))
  (left-y [this sweep-y x]
    (if (nil? left) -Infinity (bp/point-y (:pp left) sweep-y x)))
  (right-y [this sweep-y x]
    (if (nil? right) -Infinity (bp/point-y (:pp right) sweep-y x)))
  (left-bound [this sweep-y]
    (if (nil? left) nil-left-bound-point (break-point-point left sweep-y)))
  (right-bound [this sweep-y]
    (if (nil? right) nil-right-bound-point (break-point-point right sweep-y))))

(defn new-first-arc [point]
  (->Arc point nil nil (:y point)))

(defn new-arc [left right y]
  (let [point (if-not (nil? left)
                (:right left)
                (:left right))]
    (->Arc point left right y)))

(defn arc-points [arc sweep-y]
  [(left-bound arc sweep-y)
   (right-bound arc sweep-y)])

(defn q-larger
  "
  @param {!app.point.Point} q
  @param {!Arc} nq
  @param {!number} y
  @return {!boolean}
  "
  [q nq y]
  (let [nqlx (left-x nq y)
        qx (:x q)
        ret (and (< nqlx qx)
                 (not (close nqlx qx))
                 (let [nqrx (right-x nq y)]
                   (or
                     (< nqrx qx)
                     (close nqrx qx))))]
    ret))

(defn arcs-comparator
  "
  @param {!Arc} a
  @param {!Arc} b
  @return {!number}
  "
  [a b y]
  (let [alx (left-x a y)
        blx (left-x b y)
        l-close (close alx blx)]
    (if-not l-close
      (if (< alx blx) -1 1)
      (let [arx (right-x a y)
            brx (right-x b y)
            r-close (close arx brx)
            res (cond
                  (and l-close r-close)
                  (let [al (left-bound a y)
                        bl (left-bound b y)
                        aCcw (ccw (update al :y + 1000) al (:point a))
                        bCcw (ccw (update bl :y + 1000) bl (:point b))
                        oCcw (ccw (:point a) al (:point b))]
                    (if (and (not= aCcw bCcw)
                             (not= aCcw 0)
                             (not= bCcw 0))
                      (compare aCcw bCcw)
                      (cond
                        (and (nil? (:left a))
                             (some? (:left b))) -1
                        (and (nil? (:left b))
                             (some? (:left a))) 1
                        (not= 0 oCcw) oCcw
                        :else (x-ordered-comparator
                                (:point a) (:point b)))))
                  :else (let [al (left-bound a y)
                              bl (left-bound b y)
                              ar (right-bound a y)
                              br (right-bound b y)
                              mpa (midpoint al ar)
                              mpb (midpoint bl br)
                              c (x-ordered-comparator mpa mpb)]
                          c))]
        res))))

(defn arc-comparator
  "
  @param {!Object} a
  @param {!Object} b
  @return {!number}
  "
  [a b]
  (if (= a b)
    0
    (let [aq (:query a)
          bq (:query b)
          isQuery (or aq bq)
          y (if isQuery
              (if aq (:y a) (:y b))
              (max (:added-at a) (:added-at b)))]
      (cond
        aq (if (q-larger a b y) 1 -1)
        bq (if (q-larger b a y) -1 1)
        :else (arcs-comparator a b y)))))
