(ns voronoi.arc
  (:require [voronoi.util :refer [Infinity -Infinity close]]
            [voronoi.point :refer [->Point map->Point
                                   y-ordered-epsilon-comparator
                                   x-ordered-comparator
                                   ccw midpoint]]
            [voronoi.edge :refer [intersect-edges]]
            [voronoi.break-point :refer [break-point-point]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arcs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ArcComparable
  (left-bound [this sweep-y])
  (right-bound [this sweep-y]))

(def nil-left-bound-point
  (->Point -Infinity -Infinity))

(def nil-right-bound-point
  (->Point Infinity -Infinity))

(defrecord Arc [point left right added-at]
  ArcComparable
  (left-bound [this sweep-y]
    (if (nil? left)
      nil-left-bound-point
      (break-point-point left sweep-y)))
  (right-bound [this sweep-y]
    (if (nil? right)
      nil-right-bound-point
      (break-point-point right sweep-y))))

(defn new-first-arc [point]
  (->Arc point nil nil (:y point)))

(defn new-arc [left right y]
  (let [point (if-not (nil? left)
                (:right left)
                (:left right))]
    (->Arc point left right y)))

(defn left-point [arc sweep-y]
  (if (nil? (:left arc))
    (->Point -Infinity -Infinity)
    (break-point-point (:left arc) sweep-y)))

(defn arc-right-point [arc sweep-y]
  (if (nil? (:right arc))
    (->Point Infinity -Infinity)
    (break-point-point (:right arc) sweep-y)))

(defn arc-points [arc sweep-y]
  [(left-bound arc sweep-y)
   (right-bound arc sweep-y)])

(defn check-circle [arc]
  (let [l (:left arc)
        r (:right arc)
        haveNil (or (nil? l) (nil? r))
        ccwv (if-not haveNil
               (ccw (:left l) (:point arc) (:right r)))
        ]
    (cond
      haveNil nil
      (not (= 1 ccwv)) nil
      :else (intersect-edges (:edge l) (:edge r)))))


(defn arc-comp-points [arc y]
  (if (:query arc)
    [arc arc]
    (arc-points arc y)))

(defn q-less [ql qr nql nqr]
  (and (< (:x nql) (:x ql))
       (or
        (< (:x nqr) (:x qr))
        (close (:x nqr) (:x ql)))
       (not (close (:x nql) (:x ql)))))

(defn arc-comparator [a b]
  (if (= a b) 0
      (let [aq (:query a)
            bq (:query b)
            isQuery (or aq bq)
            y (if isQuery
                (if aq (:y a) (:y b))
                (max (:added-at a) (:added-at b)))
            al (if aq a (left-bound a y))
            ar (if aq a (right-bound a y))
            bl (if bq b (left-bound b y))
            br (if bq b (right-bound b y))
            res (cond
                  aq (if (q-less al ar bl br) 1 -1)
                  bq (if (q-less bl br al ar) -1 1)
                  (and (close (:x al) (:x bl))
                       (close (:x ar) (:x br)))
                  (let [aCcw (ccw (update al :y + 1000) al (:point a))
                        bCcw (ccw (update bl :y + 1000) bl (:point  b))
                        oCcw (ccw (:point a) al (:point b))
                        ccwv (ccw (:point a) (:point b) al)]
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
                  :else (let [mpa (midpoint al ar)
                              mpb (midpoint bl br)
                              c (x-ordered-comparator mpa mpb)]
                          c))]
        res)))
