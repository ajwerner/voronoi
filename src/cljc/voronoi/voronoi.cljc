(ns voronoi.voronoi
  (:require [clojure.string :as str]
            [voronoi.util :refer [Infinity -Infinity sqrt isNaN?]]
            [voronoi.basic-geometry :refer [sq abs close distance]]
            [voronoi.point :refer [->Point map->Point
                                   y-ordered-epsilon-comparator
                                   x-ordered-comparator
                                   ccw midpoint]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Break Points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord BreakPoint [left right edge side begin])

(defn break-point-point [p sweep-y]
  (let [l (:left p)
        r (:right p)]
    (if (close (:y l) (:y r))
      ;; vertical line case
      (let [x (/ (+ (:x l) (:x r))
                 2)
            y (/ (+ (sq (- x (:x l)))
                    (sq (:y l))
                    (* -1 (sq sweep-y)))
                 (* 2 (- (:y l) sweep-y)))
            y (if (= y Infinity) -Infinity y)]
        (->Point x y))
      (let [px (:x l)
            py (:y l)
            m (:m (:edge p))
            b (:b (:edge p))
            d (* 2 (- py sweep-y))
            A 1
            B (- (* -2 px) (* d m))
            C (- (+ (sq px) (sq py))
                 (sq sweep-y)
                 (* d b))
            descrim (+ (sq B) (* -4 A C))
            x (if (<= descrim 0) ;; deal with near zero precision cases
                (/ (* -1 B) (* 2 A))
                (let [num (- (* -1 B) (sqrt descrim))]
                  (if (> (:y l) (:y r))
                    ;; if left, use more precise float logic by rationalizing
                    ;; the denominator
                    (/ (* 2 C) num)
                    (/ num (* 2 A)))))
            y (+ (* m x) b)]
        (->Point x y)))))

(defn new-break-point [left right edge side y]
  (let [p (->BreakPoint left right edge side 0)
        p (assoc p :begin (break-point-point p y))]
    p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edges
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord HalfEdge [site1 site2 m b is-vertical])

(defn new-edge [s1 s2]
  (let [is-vertical (close (:y s1) (:y s2))
        [m b] (if is-vertical
                [Infinity 0]
                (let [m (/ -1
                           (/ (- (:y s1) (:y s2))
                              (- (:x s1) (:x s2))))
                      mid (midpoint s1 s2)
                      b (- (:y mid)
                           (* m (:x mid)))]
                  [m b]))]
    (->HalfEdge s1 s2 m b is-vertical)))

(defn vertical-intersection [vert-edge non-vert-edge]
  (let [x (/ (+ (:x (:site1 vert-edge))
                (:x (:site2 vert-edge)))
             2)
        y (+ (* (:m non-vert-edge) x)
             (:b non-vert-edge))]
    (->Point x y)))

(defn intersect-edges [e1 e2]
  (let [v1 (:is-vertical e1)
        v2 (:is-vertical e2)]
    (cond
      (and v1 v2) nil
      v1 (vertical-intersection e1 e2)
      v2 (vertical-intersection e2 e1)
      (and (== (:m e1) (:m e2))
           (not (== (:b e1) (:b e2)))) nil
      :else (let [x (/ (- (:b e2) (:b e1))
                       (- (:m e1) (:m e2)))
                  y (+ (* (:m e1) x)
                       (:b e1))]
              (->Point x y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arcs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Arc [point left right added-at])

(defn new-arc [left right y]
  (let [point (if-not (nil? left)
                (:right left)
                (:left right))
        ]
    (->Arc point left right y)))

(defn arc-left-point [arc sweep-y]
  (if (nil? (:left arc))
    (->Point -Infinity -Infinity)
    (break-point-point (:left arc) sweep-y)))

(defn arc-right-point [arc sweep-y]
  (if (nil? (:right arc))
    (->Point Infinity -Infinity)
    (break-point-point (:right arc) sweep-y)))

(defn arc-points [arc sweep-y]
  (map #(% arc sweep-y) [arc-left-point arc-right-point]))

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

(defn arc-comparator [a b]
  (if (= a b) 0
      (let [aq (:query a)
            bq (:query b)
            isQuery (or aq bq)
            y (if isQuery
                (if aq (:y a) (:y b))
                (max (:added-at a) (:added-at b)))
            [{alx :x aly :y :as al}
             {arx :x ary :y :as ar}] (arc-comp-points a y)
            [bl br] (arc-comp-points b y)
            q-less (fn [[ql qr :as q] [nql nqr :as nq]]
                    (and (< (:x nql) (:x ql))
                            (or
                             (< (:x nqr) (:x qr))
                             (close (:x nqr) (:x ql)))
                            (not (close (:x nql) (:x ql)))))
            res (cond
                  aq (if (q-less [al ar] [bl br]) 1 -1)
                  bq (if (q-less [bl br] [al ar]) -1 1)
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
                          c))
            ]
        res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord CircleEvent [x y vert arc])

(defn is-circle [ev] (instance? CircleEvent ev))

(defn event-comparator [a b]
  (let [ay (:y a)
        by (:y b)
        c (if-not (close ay by)
            (compare ay by))]
    (if (and (some? c) (not= c 0))
      c
      (let [aCircle (is-circle a)
            bCircle (is-circle b)
            aCcw (ccw a (:vert a) (:point (:arc a)))
            bCcw (ccw b (:vert b) (:point (:arc b)))
            oCcw (ccw (:point (:arc a)) (:vert a) (:point (:arc b)))
            cx (compare (:x a) (:x b))
            weVert (if (and aCircle bCircle)
                     (close (:x (:vert a)) (:x (:vert b))))
            breaker (cond
                      (not (or aCircle bCircle)) cx
                      (and aCircle (not bCircle)) -1
                      (and bCircle (not aCircle)) 1
                      :else (arc-comparator (:arc a) (:arc b)))]
        breaker))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Voronoi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn build-points [initial-points]
  (loop [to-process initial-points
         processed (sorted-set-by y-ordered-epsilon-comparator)]
    (let [cur (first to-process)
          exists (contains? processed cur)
          processed (if-not exists (processed cur) processed)
          ]
      (recur (rest to-process) processed))))


(defrecord VoronoiBuilder [input points scan events edges complete breaks arcs])

(defn new-voronoi [input]
  "Returns a map representing a voronoi builder"
  (let [points (map map->Point input)
        events (into (sorted-set-by event-comparator) points)
        scan (:y (first events))]
    (map->VoronoiBuilder {:input input
                          :points points
                          :scan scan
                          :events events
                          :edges []
                          :completed []
                          :breaks #{}
                          :arcs (sorted-map-by arc-comparator)})))

(defn check-for-circle-event [vor arc]
  (if-let [center (check-circle arc)]
    (let [rad (distance (:point arc) center)
          x (:x center)
          y (+ (:y center) rad)
          ev (->CircleEvent x y center arc)]
      (-> vor
          (update :events #(conj % ev))
          (update :arcs #(assoc % arc ev))))
    vor))

(defn handle-circle-event [{arcs :arcs :as vor} {y :y arc :arc :as ev}]
  (let [
        arcLeftEntry (if-let [s (rsubseq arcs < arc)] (first s))
        arcLeft (key arcLeftEntry)
        arcRightEntry (if (some? arcLeft)
                        (second (subseq arcs > arcLeft))
                        (second arcs))
        arcRight (key arcRightEntry)
        evLeft (val arcLeftEntry)
        evRight (val arcRightEntry)
        newEdge (new-edge (:left (:left arc))
                          (:right (:right arc)))
        turnsLeft (== 1 (ccw (:begin (:right arcLeft))
                             ev
                             (:begin (:left arcRight))))
        isLeftPoint (if turnsLeft
                      (< (:m newEdge) 0)
                      (> (:m newEdge) 0))
        newEdge (if isLeftPoint
                  (assoc newEdge :p1 (:vert ev))
                  (assoc newEdge :p2 (:vert ev)))
        side (if (not isLeftPoint) :left :right)
        bp (new-break-point (:left (:left arc))
                            (:right (:right arc))
                            newEdge side y)
        newArcRight (new-arc bp (:right arcRight) y)
        newArcLeft (new-arc (:left arcLeft) bp y)]
    (-> vor
        (assoc :scan y)
        (update :events #(disj % evLeft evRight))
        (update :edges #(conj % newEdge))
        (update :arcs #(dissoc % arc arcRight arcLeft))
        (update :arcs #(assoc % newArcRight nil newArcLeft nil))
        (update :breaks #(disj % (:left arc) (:right arc)))
        (update :breaks #(conj % bp))
        (update :completed #(conj %
                                  {:end (:vert ev)
                                     :begin (:begin (:left arc))
                                     :edge (:edge (:left arc))}
                                  {:end (:vert ev)
                                   :begin (:begin (:right arc))
                                   :edge (:edge (:right arc))}))
        (check-for-circle-event newArcLeft)
        (check-for-circle-event newArcRight))))

(defn handle-later-site-event
  "Used after the first site event where things are weird."
  [{edges :edges
    breaks :breaks
    arcs :arcs
    scan :scan
    events :events
    :as vor}
   {x :x y :y :as ev}]
  (let [step1 (rsubseq arcs < (assoc ev :query true))
        step2 (if (not-empty step1)
                (first (subseq arcs > (key (first step1))))
                (first arcs))
        queryRes  step2
        arcAboveEntry step2
        queryRes2 (subseq arcs >= (assoc ev :query true))
        [arcAbove falseCircleEvent] (if (some? arcAboveEntry)
                                      [(key arcAboveEntry) (val arcAboveEntry)]
                                      [nil nil])
        newEdge (new-edge (:point arcAbove) ev)
        breakL (:left arcAbove)
        breakR (:right arcAbove)
        newBreakL (new-break-point (:point arcAbove) ev newEdge :left y)
        newBreakR (new-break-point ev (:point arcAbove) newEdge :right y)
        ivl (:is-vertical (:edge newBreakL))
        ivr (:is-vertical (:edge newBreakR))
        sameX (close (:x (:begin newBreakR))
                     (:x (:begin newBreakL)))
        newVertical (and ivl ivr sameX)
        newBreakR (if (and newVertical
                           (isNaN? (:y (:begin newBreakR))))
                    (assoc-in newBreakR [:begin :y] (:y (:begin breakR)))
                    newBreakR)
        newVertBreak (if-not newVertical
                       nil
                       (new-break-point (:point arcAbove) ev newEdge :vert y))

        eventIsOnVerticalLine (and (= :vert
                                      (:side (:edge breakR)))
                                   (close  (:x ev) (:x (:begin (:edge breakR)))))
        newVertBreak (if (and newVertical
                              (isNaN? (:y (:begin newVertBreak))))
                       (assoc-in newVertBreak [:begin :y] (:y (:begin breakL)))
                       newVertBreak)
        [arcLeft
         arcCenter
         arcRight] (if newVertical
                     [(new-arc breakL newVertBreak y)
                      nil
                      (new-arc newVertBreak breakR y)]
                     [(new-arc breakL newBreakL y)
                      (new-arc newBreakL newBreakR y)
                      (new-arc newBreakR breakR y)])]
    (-> vor
        (assoc :scan y)
        (assoc :events (if falseCircleEvent
                         (disj events falseCircleEvent)
                         events)
               :edges (conj edges newEdge)
               :breaks (if newVertical
                         (conj breaks newVertBreak)
                         (conj breaks newBreakL newBreakR))
               :arcs (-> arcs
                         (dissoc arcAbove)
                         (assoc arcLeft nil
                                arcCenter nil
                                arcRight nil)))
        (check-for-circle-event arcLeft)
        (check-for-circle-event arcRight))))

(defn handle-site-event [vor ev]
  (if (= 0 (count (:arcs vor)))
    (-> vor
        (update :arcs #(assoc % {:point ev} nil))
        (assoc :scan (:y ev)))
    (handle-later-site-event vor ev)))

(defn handle-event [{events :events :as vor}]
  (let [ev (first events)
        handler (if (is-circle ev)
                  handle-circle-event
                  handle-site-event)]
    (if-not ev vor
            (-> vor
                (update :events #(disj % ev))
                (handler ev)))))

(defn scan-to [vor to]
  (loop [{events :events :as cur} vor]
    (let [ev (first events)
          have-event (and (some? ev)
                          (>= to (:y ev)))]
      (if-not have-event
          (assoc cur :scan to)
          (recur (handle-event cur))))))

(defn reset-to [vor y]
  (scan-to (new-voronoi (:points vor)) y))
