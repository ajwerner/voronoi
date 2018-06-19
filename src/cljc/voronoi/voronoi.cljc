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
    [input points scan events edges complete breaks arcs ])

(defn new-voronoi [input]
  "Returns a map representing a voronoi builder"
  (let [points (map point/map->Point input)
        events (into (sorted-set-by event/event-comparator) points)
        scan (:y (first events))]
    (map->VoronoiBuilder {:input input
                          :points points
                          :scan scan
                          :events events
                          :edges []
                          :completed []
                          :breaks #{}
                          :arcs (sorted-map-by arc/arc-comparator)})))

(defn check-for-circle-event [vor arc]
  (if-let [center (arc/check-circle arc)]
    (let [rad (point/distance (:point arc) center)
          x (:x center)
          y (+ (:y center) rad)
          ev (event/->CircleEvent x y center arc)]
      (-> vor
          (update :events conj ev)
          (update :arcs assoc arc ev)))
    (update vor :arcs assoc arc nil)))

(defn handle-circle-event [{arcs :arcs
                            :as vor}
                           {y :y
                            arc :arc
                            :as ev}]
  (let [
        arc-left-entry (if-let [s (rsubseq arcs < arc)] (first s))
        arc-left (key arc-left-entry)
        arc-right-entry (if (some? arc-left)
                          (second (subseq arcs > arc-left))
                          (second arcs))
        arc-right (key arc-right-entry)
        ev-left (val arc-left-entry)
        ev-right (val arc-right-entry)
        new-h-e (edge/new-half-edge (:left (:left arc))
                                    (:right (:right arc)))
        turns-left (== 1 (point/ccw (:begin (:right arc-left))
                                   ev
                                   (:begin (:left arc-right))))
        is-left-point (if turns-left
                      (< (:m new-h-e) 0)
                      (> (:m new-h-e) 0))
        new-h-e (if is-left-point
                  (assoc new-h-e :p1 (:vert ev))
                  (assoc new-h-e :p2 (:vert ev)))
        side (if (not is-left-point) :left :right)
        bp (bp/new-break-point (:left (:left arc))
                               (:right (:right arc))
                               new-h-e side y)
        new-arc-right (arc/new-arc bp (:right arc-right) y)
        new-arc-left (arc/new-arc (:left arc-left) bp y)]
    (-> vor
        (assoc :scan y)
        (update :events #(apply disj % (remove nil? [ev-left ev-right])))
        (update :edges conj new-h-e)
        (update :arcs dissoc arc arc-left arc-right)
        (update :breaks disj (:left arc) (:right arc))
        (update :breaks conj bp)
        (update :completed conj
                {:end (:vert ev)
                 :begin (:begin (:left arc))
                 :edge (:edge (:left arc))}
                {:end (:vert ev)
                 :begin (:begin (:right arc))
                 :edge (:edge (:right arc))})
        (check-for-circle-event new-arc-left)
        (check-for-circle-event new-arc-right))))

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
        arc-above-entry step2
        arc-above (if (some? arc-above-entry) (key arc-above-entry))
        false-circle-event (if (some? arc-above-entry) (val arc-above-entry))
        new-h-e (edge/new-half-edge (:point arc-above) ev)
        bp-l (:left arc-above)
        bp-r (:right arc-above)
        new-bp-l (bp/new-break-point (:point arc-above) ev new-h-e :left y)
        new-bp-r (bp/new-break-point ev (:point arc-above) new-h-e :right y)
        ivl (:is-vertical (:edge new-bp-l))
        ivr (:is-vertical (:edge new-bp-r))
        same-x (close (:x (:begin new-bp-l))
                      (:x (:begin new-bp-r)))
        new-vertical (and ivl ivr same-x)
        new-bp-r (if (and new-vertical
                             (isNaN? (:y (:begin new-bp-r))))
                      (assoc-in new-bp-r [:begin :y] (:y (:begin bp-r)))
                      new-bp-r)
        new-vert-bp (if-not new-vertical
                      nil
                      (bp/new-break-point (:point arc-above) ev new-h-e :vert y))
        event-is-on-vert-line (and (= :vert
                                      (:side (:edge bp-r)))
                                   (close  (:x ev) (:x (:begin (:edge bp-r)))))
        new-vert-bp (if (and new-vertical
                             (isNaN? (:y (:begin new-vert-bp))))
                      (assoc-in new-vert-bp [:begin :y] (:y (:begin bp-l)))
                      new-vert-bp)
        arc-left (if new-vertical
                   (arc/new-arc bp-l new-vert-bp y)
                   (arc/new-arc bp-l new-bp-l y))
        arc-center (if new-vertical
                     nil
                     (arc/new-arc new-bp-l new-bp-r y))
        arc-right (if new-vertical
                    (arc/new-arc new-vert-bp bp-r y)
                    (arc/new-arc new-bp-r bp-r y))]
    (-> vor
        (assoc :scan y)
        (assoc :events (if false-circle-event
                         (disj events false-circle-event)
                         events)
               :edges (conj edges new-h-e)
               :breaks (if new-vertical
                         (conj breaks new-vert-bp)
                         (conj breaks new-bp-l new-bp-r))
               :arcs (as-> arcs arcs
                       (dissoc arcs arc-above)
                       (if (some? arc-center)
                         (assoc arcs arc-center nil) arcs)))
        (check-for-circle-event arc-left)
        (check-for-circle-event arc-right))))

(defn handle-first-site-event [vor ev]
  (-> vor
      (update :arcs assoc (arc/new-first-arc ev) nil)
      (assoc :scan (:y ev))))

(defn handle-site-event [vor ev]
  (if (= 0 (count (:arcs vor)))
    (handle-first-site-event vor ev)
    (handle-later-site-event vor ev)))

(defn handle-event [{events :events :as vor}]
  (let [ev (first events)
        handler (if (event/is-circle ev)
                  handle-circle-event
                  handle-site-event)]
    (if-not ev vor
            (-> vor
                (update :events disj ev)
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

(defn process-all-events [vor]
  (loop [vor vor]
    (if (empty? (:events vor))
      vor
      (recur (handle-event vor)))))

(defn finish [vor]
  (let [vor (process-all-events vor)]
    vor))
