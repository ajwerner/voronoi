(ns voronoi.voronoi
  (:require [clojure.string :as str]
            [voronoi.util :refer [Infinity -Infinity sqrt isNaN? close]]
            [voronoi.point :as point]
            [voronoi.arc :as arc]
            [voronoi.break-point :as bp]
            [voronoi.edge :as edge]
            [voronoi.event :as event]))

(defn build-points [initial-points]
  (loop [to-process initial-points
         processed (sorted-set-by point/y-ordered-epsilon-comparator)]
    (let [cur (first to-process)
          exists (contains? processed cur)
          processed (if-not exists (processed cur) processed)]
      (recur (rest to-process) processed))))

(defrecord VoronoiBuilder
    [input points scan events edges complete breaks arcs])

(defn new-voronoi [input]
  "Returns a map representing a voronoi builder"
  (let [points (map point/map->Point input)
        events (into (sorted-set-by event/comparator) points)
        scan (:y (first events))]
    (map->VoronoiBuilder {:input input
                          :points points
                          :scan scan
                          :events events
                          :edges []
                          :completed []
                          :breaks #{}
                          :arcs (sorted-map-by arc/comparator)})))

(defn check-for-circle-event [vor arc]
  (if-let [center (arc/check-circle arc)]
    (let [rad (point/distance (:point arc) center)
          x (:x center)
          y (+ (:y center) rad)
          ev (event/->CircleEvent x y center arc)]
      (-> vor
          (update :events #(conj % ev))
          (update :arcs #(assoc % arc ev))))
    (update vor :arcs #(assoc % arc nil))))

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
        new-h-e (edge/new-half-edge (:left (:left arc))
                                    (:right (:right arc)))
        turnsLeft (== 1 (point/ccw (:begin (:right arcLeft))
                                   ev
                                   (:begin (:left arcRight))))
        isLeftPoint (if turnsLeft
                      (< (:m new-h-e) 0)
                      (> (:m new-h-e) 0))
        new-h-e (if isLeftPoint
                  (assoc new-h-e :p1 (:vert ev))
                  (assoc new-h-e :p2 (:vert ev)))
        side (if (not isLeftPoint) :left :right)
        bp (bp/new-break-point (:left (:left arc))
                               (:right (:right arc))
                               new-h-e side y)
        newArcRight (arc/new-arc bp (:right arcRight) y)
        newArcLeft (arc/new-arc (:left arcLeft) bp y)]
    (-> vor
        (assoc :scan y)
        (update :events #(disj % evLeft evRight))
        (update :edges #(conj % new-h-e))
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
        new-h-e (edge/new-half-edge (:point arcAbove) ev)
        breakL (:left arcAbove)
        breakR (:right arcAbove)
        newBreakL (bp/new-break-point (:point arcAbove) ev new-h-e :left y)
        newBreakR (bp/new-break-point ev (:point arcAbove) new-h-e :right y)
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
                       (bp/new-break-point (:point arcAbove) ev new-h-e :vert y))

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
                     [(arc/new-arc breakL newVertBreak y)
                      nil
                      (arc/new-arc newVertBreak breakR y)]
                     [(arc/new-arc breakL newBreakL y)
                      (arc/new-arc newBreakL newBreakR y)
                      (arc/new-arc newBreakR breakR y)])]
    (-> vor
        (assoc :scan y)
        (assoc :events (if falseCircleEvent
                         (disj events falseCircleEvent)
                         events)
               :edges (conj edges new-h-e)
               :breaks (if newVertical
                         (conj breaks newVertBreak)
                         (conj breaks newBreakL newBreakR))
               :arcs (as-> arcs arcs
                       (dissoc arcs arcAbove)
                       (if (some? arcCenter) (assoc arcs arcCenter nil) arcs)))
        (check-for-circle-event arcLeft)
        (check-for-circle-event arcRight))))

(defn handle-site-event [vor ev]
  (if (= 0 (count (:arcs vor)))
    (-> vor
        (update :arcs #(assoc % (arc/new-first-arc ev) nil))
        (assoc :scan (:y ev)))
    (handle-later-site-event vor ev)))

(defn handle-event [{events :events :as vor}]
  (let [ev (first events)
        handler (if (event/is-circle ev)
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
