(ns voronoi.builder
  (:require [clojure.data.avl :as avl]
            [voronoi.util :refer [Infinity -Infinity sqrt isNaN? close
                                  is-infinite?]]
            [voronoi.point :as point]
            [voronoi.arc :as arc]
            [voronoi.clip :as clip]
            [voronoi.break-point :as bp]
            [voronoi.edge :as edge]
            [voronoi.event :as event]))


;; the terminology throughout this implementation can be a little confusing
;; because relative directions assume an orientation.
;; The orientation assumed is that (0, 0) is in the top left corner and
;; y-increases going down. This can be horribly confusing. I regret it.

;; we want to write a thing to make a list of points

(defn check-circle [arc]
  (let [l (:left arc)
        r (:right arc)
        have-nil (or (nil? l) (nil? r))
        ccwv (if-not have-nil
               (point/ccw (:left l) (:point arc) (:right r)))]
    (cond
      have-nil nil
      (not (= 1 ccwv)) nil
      :else (edge/intersect-edges (:edge l) (:edge r)))))

(defn circle-event [arc]
  (if-let [center (check-circle arc)]
    (let [rad (point/distance (:point arc) center)
          x (:x center)
          y (+ (:y center) rad)
          ev (event/->CircleEvent x y center arc)]
      ev)))

(defn make-persistent! [{:keys [edges] :as vor}]
  (cond-> vor
          true (assoc! :edges (persistent! edges))
          true (persistent!)))

(defn make-transient [{:keys [edges] :as vor}]
  (cond-> vor
          true (transient)
          true (assoc! :edges (transient edges))))

(defn ^:private handle-circle-event!
  [{arcs :arcs
    events :events
    edges :edges
    :as vor}
   {y :y
    arc :arc
    :as ev}]
  (let [r (avl/rank-of arcs arc)
        [arc-left ev-left] (if (> r 0)
                             (nth arcs (- r 1))
                             [nil nil])
        [arc-right ev-right] (if (< r (count arcs))
                               (nth arcs (+ r 1))
                               [nil nil])
        bp-left (:left (:left arc))
        bp-right (:right (:right arc))
        new-h-e (edge/new-half-edge bp-left bp-right)
        side (if (<= (:y bp-left) (:y bp-right)) :left :right)
        new-h-e (if (not= side :left)
                  (assoc new-h-e :p1 (:vert ev))
                  (assoc new-h-e :p2 (:vert ev)))
        bp (bp/new-break-point (:left (:left arc))
                               (:right (:right arc))
                               new-h-e side y (:vert ev))
        new-arc-left (arc/new-arc (:left arc-left) bp y)
        new-arc-right (arc/new-arc bp (:right arc-right) y)
        ce-l (circle-event new-arc-left)
        ce-r (circle-event new-arc-right)
        events (cond-> events
                       ev-left (disj ev-left)
                       ev-right (disj ev-right)
                       ce-l (conj ce-l)
                       ce-r (conj ce-r))
        arcs (cond-> arcs
                     true (transient)
                     arc-left (dissoc! arc-left)
                     true (dissoc! arc)
                     arc-right (dissoc! arc-right)
                     true (assoc! new-arc-left ce-l)
                     true (assoc! new-arc-right ce-r)
                     true (persistent!))
        edges (-> edges
                  (conj! (edge/new-complete (:left arc) (:vert ev)))
                  (conj! (edge/new-complete (:right arc) (:vert ev))))]
    (->  vor
         (assoc! :scan y)
         (assoc! :events events)
         (assoc! :arcs arcs)
         (assoc! :edges edges))))

(defn ^:private handle-later-site-event!
  "Used after the first site event where things are weird."
  [{arcs :arcs
    events :events
    edges :edges
    :as vor}
   {y :y :as ev}]
  (let [[l _] (avl/nearest arcs < (assoc ev :query true))
        [arc-above
         false-circle]  (if (and (> (count arcs) 1)
                                 (some? l))
                          (avl/nearest arcs > l)
                          (first arcs))
        new-h-e (edge/new-half-edge (:point arc-above) ev)
        bp-l (:left arc-above)
        bp-r (:right arc-above)
        new-bp-l (bp/new-break-point (:point arc-above) ev new-h-e
                                     :left y nil)
        new-bp-r (bp/new-break-point ev (:point arc-above) new-h-e
                                     :right y (:begin new-bp-l))
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
                      (bp/new-break-point (:point arc-above) ev new-h-e
                                          :vert y (point/->Point
                                                    (:x (:begin new-bp-r))
                                                    -Infinity)))
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
                    (arc/new-arc new-bp-r bp-r y))
        ce-r (circle-event arc-right)
        ce-l (circle-event arc-left)
        arcs (cond-> arcs
                     true (transient)
                     true (dissoc! arc-above)
                     true (assoc! arc-left ce-l)
                     true (assoc! arc-right ce-r)
                     arc-center (assoc! arc-center nil)
                     true (persistent!))
        events (cond-> events
                       false-circle (disj false-circle)
                       ce-r (conj ce-r)
                       ce-l (conj ce-l))]
    (-> vor
        (assoc! :scan y)
        (assoc! :edges edges)
        (assoc! :events events)
        (assoc! :arcs arcs))))

(defn ^:private handle-first-site-event! [vor ev]
  (let [arcs (assoc (:arcs vor) (arc/new-first-arc ev) nil)]
    (-> vor
        (assoc! :arcs arcs)
        (assoc! :scan (:y ev)))))

(defn ^:private handle-site-event! [vor ev]
  (if (= 0 (count (:arcs vor)))
    (handle-first-site-event! vor ev)
    (handle-later-site-event! vor ev)))

(defn ^:private handle-event! [vor ev]
  (let [events (disj (:events vor) ev)
        vor (assoc! vor :events events)]
    (if (event/is-circle ev)
      (handle-circle-event! vor ev)
      (handle-site-event! vor ev))))

(defn handle-event [{events :events :as vor}]
  (let [ev (first events)]
    (if ev
      (-> vor
          (make-transient)
          (handle-event! ev)
          (make-persistent!))
      vor)))

(defn process-all-events! [vor]
  (loop [{events :events :as vor} vor]
    (if-let [ev (first events)]
      (recur (handle-event! vor ev))
      vor)))

(defn process-all-events [vor]
  (make-persistent! (process-all-events! (make-transient vor))))

(defn ^:private scan-to! [vor to]
  (loop [{events :events :as cur}  vor]
    (let [ev (first events)
          have-event (and (some? ev)
                          (>= to (:y ev)))]
      (if-not have-event
        (assoc! cur :scan to)
        (recur (handle-event! cur ev))))))

(defn scan-by! [vor by]
  (scan-to! vor (+ (:scan vor) by)))

(defn scan-to [vor to]
  (make-persistent! (scan-to! (make-transient vor) to)))

(defn ^:private create-points-from-input
  [input extent]
  (->> input
       (remove #(clip/outside-extent? extent %))
       (map point/map->Point)))

(defn new-builder
  "Returns a map representing a app builder.
  If no extent is specified, a default extent 40% larger than
  the bounding box of the points will be used unless no-extent is specified.
  "
  [input & {:keys [extent no-extent]}]
  (let [points (create-points-from-input input extent)
        extent (if extent extent
                          (if (not no-extent)
                            (-> points
                                (point/bound-box)
                                (point/widen-by-percent 40))))
        events (into (sorted-set-by event/event-comparator) points)
        scan (:y (first events))]
    {:input input
     :points points
     :scan scan
     :events events
     :extent extent
     :edges []
     :arcs (avl/sorted-map-by arc/arc-comparator)}))

(defn reset-to [vor y]
  (scan-to (new-builder (:points vor)) y))
