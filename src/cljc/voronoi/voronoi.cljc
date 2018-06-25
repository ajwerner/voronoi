(ns voronoi.voronoi
  (:require [clojure.string :as str]
            [clojure.data.avl :as avl]
            [voronoi.util :refer [Infinity -Infinity sqrt isNaN? close is-infinite?]]
            [voronoi.point :as point]
            [voronoi.arc :as arc]
            [voronoi.break-point :as bp]
            [voronoi.edge :as edge]
            [voronoi.event :as event]))

;; the terminology throughout this implementation can be a little confusing
;; because relative directions assume an orientation.
;; The orientation assumed is that (0, 0) is in the top left corner and
;; y-increases going down. This can be horribly confusing. I regret it.


(defn build-points [initial-points]
  (loop [to-process initial-points
         processed (sorted-set-by point/y-ordered-epsilon-comparator)]
    (let [cur (first to-process)
          exists (contains? processed cur)
          processed (if-not exists (processed cur) processed)]
      (recur (rest to-process) processed))))

;; we want to write a thing to make a list of points

(defn outside-extent? [[xmin xmax ymin ymax :as extent] {x :x y :y}]
  (let [p (and (some? extent)
               (or
                (< x xmin)
                (> x xmax)
                (< y ymin)
                (> y ymax)))]
    p))

(defn create-points-from-input [input extent]
  (->> input
      (remove #(outside-extent? extent %))
      (map point/map->Point)))

(defn new-voronoi
  "Returns a map representing a voronoi builder"
  [input & {:keys [extent]
            }]
  (let [points (create-points-from-input input extent)
        extent (if extent extent
                   (-> points
                       (point/bound-box)
                       (point/widen-by-percent 40)))
        events (into (sorted-set-by event/event-comparator) points)
        scan (:y (first events))]
    {:input input
     :points points
     :scan scan
     :events events
     :extent extent
     :edges []
     :completed []
     :breaks #{}
     :arcs (avl/sorted-map-by arc/arc-comparator)}))

(defn check-circle [arc]
  (let [l (:left arc)
        r (:right arc)
        haveNil (or (nil? l) (nil? r))
        ccwv (if-not haveNil
               (point/ccw (:left l) (:point arc) (:right r)))]
    (cond
      haveNil nil
      (not (= 1 ccwv)) nil
      :else (edge/intersect-edges (:edge l) (:edge r)))))

(defn circle-event [arc]
  (if-let [center (check-circle arc)]
    (let [rad (point/distance (:point arc) center)
          x (:x center)
          y (+ (:y center) rad)
          ev (event/->CircleEvent x y center arc)]
      ev)
    nil))

(defn make-persistent! [{:keys [arcs events breaks completed edges] :as vor}]
  (cond-> vor
    (some? breaks)  (assoc! :breaks (persistent! breaks))
    true (assoc! :completed (persistent! completed))
    true (assoc! :edges (persistent! edges))
    true (persistent!)))

(defn make-transient [{:keys [arcs events breaks completed edges] :as vor}]
  (cond-> vor
      true (transient)
      (some? breaks) (assoc! :breaks (transient breaks))
      true (assoc! :completed (transient completed))
      true (assoc! :edges (transient edges))))

(defn handle-circle-event!
  [{arcs :arcs
    edges :edges
    events :events
    breaks :breaks
    completed :completed
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
        side (if (< (:y bp-left) (:y bp-right)) :left :right)
        new-h-e (if (not= side :left)
                  (assoc new-h-e :p1 (:vert ev))
                  (assoc new-h-e :p2 (:vert ev)))
        bp (bp/new-break-point (:left (:left arc))
                               (:right (:right arc))
                               new-h-e side y (:vert ev))
        new-arc-left (arc/new-arc (:left arc-left) bp y)
        ce-l (circle-event new-arc-left)
        new-arc-right (arc/new-arc bp (:right arc-right) y)
        ce-r (circle-event new-arc-right)
        events (if ev-left (disj events ev-left) events)
        events (if ev-right (disj events ev-right) events)
        events (if ce-l (conj events ce-l) events)
        events (if ce-r (conj events ce-r) events)
        edges (conj! edges new-h-e)
        arcs (transient arcs)
        arcs (if (some? arc-left) (dissoc! arcs arc-left) arcs)
        arcs (dissoc! arcs arc)
        arcs (if (some? arc-right) (dissoc! arcs arc-right) arcs)
        arcs (assoc! arcs new-arc-left ce-l)
        arcs (assoc! arcs new-arc-right ce-r)
        arcs (persistent! arcs)
        breaks (if-let [l (:left arc)] (disj! breaks l) breaks)
        breaks (if-let [r (:right arc)] (disj! breaks r) breaks)
        breaks (conj! breaks bp)
        completed (conj! completed (edge/new-complete (:left arc) (:vert ev)))
        completed (conj! completed (edge/new-complete (:right arc) (:vert ev)))]
    (->  vor
         (assoc! :scan y)
         (assoc! :events events)
         (assoc! :edges edges)
         (assoc! :arcs arcs)
         (assoc! :breaks breaks)
         (assoc! :completed completed))))

(defn ^:private handle-later-site-event!
  "Used after the first site event where things are weird."
  [{edges :edges
    breaks :breaks
    arcs :arcs
    scan :scan
    events :events
    :as vor}
   {x :x y :y :as ev}]
  (let [[l _] (avl/nearest arcs < (assoc ev :query true))
        [arc-above
         false-circle]  (if (and (> (count arcs) 1)
                                 (some? l))
                          (avl/nearest arcs > l)
                          (first arcs))
        new-h-e (edge/new-half-edge (:point arc-above) ev)
        bp-l (:left arc-above)
        bp-r (:right arc-above)
        new-bp-l (bp/new-break-point (:point arc-above) ev new-h-e :left y nil)
        new-bp-r (bp/new-break-point ev (:point arc-above) new-h-e :right y (:begin new-bp-l))
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
                      (bp/new-break-point (:point arc-above) ev new-h-e :vert y nil))
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
                    (arc/new-arc new-bp-r bp-r y))
        ce-r (circle-event arc-right)
        ce-l (circle-event arc-left)
        arcs (as-> arcs arcs
               (transient arcs)
               (dissoc! arcs arc-above)
               (assoc! arcs arc-left ce-l)
               (assoc! arcs arc-right ce-r)
               (if arc-center
                 (assoc! arcs arc-center nil)
                 arcs)
               (persistent! arcs))
        events (as-> events evs
                 (if false-circle
                   (disj evs false-circle)
                   evs)
                 (if ce-r (conj evs ce-r) evs)
                 (if ce-l (conj evs ce-l) evs))
        breaks (if new-vertical
                 (conj! breaks new-vert-bp)
                 (-> breaks
                     (conj! new-bp-l)
                     (conj! new-bp-r)))]
    (-> vor
        (assoc! :scan y)
        (assoc! :events events)
        (assoc! :arcs arcs)
        (assoc! :edges (conj! edges new-h-e))
        (assoc! :breaks breaks))))

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

(defn ^:private scan-to! [vor to]
  (loop [{events :events :as cur}  vor]
    (let [ev (first events)
          have-event (and (some? ev)
                          (>= to (:y ev)))]
      (if-not have-event
        (assoc! cur :scan to)
        (recur (handle-event! cur ev))))))

(defn scan-to [vor to]
  (make-persistent! (scan-to! (make-transient vor) to)))

(defn reset-to [vor y]
  (scan-to (new-voronoi (:points vor)) y))

(defn ^:private process-all-events! [vor]
  (loop [{events :events :as vor} vor]
    (if-let [ev (first events)]
      (recur (handle-event! vor ev))
      vor)))

(defn process-all-events [vor]
  (make-persistent! (process-all-events! (make-transient vor))))

(defn add-site-edge [edge-list half-edge]
  (if edge-list
    (conj edge-list half-edge)
    [half-edge]))

(defn add-cell-edges [site-edges {half-edge :edge} i]
  (-> site-edges
      (update (:site1 half-edge) add-site-edge i)
      (update (:site2 half-edge) add-site-edge i)))

(defn make-cells [vor]
  (loop [i 0
         completed (seq (:completed vor))
         cells {}]
    (if-let [he (first completed)]
      (recur (+ i 1)
             (rest completed)
             (add-cell-edges cells he i))
      cells)))

(defn ^:private complete-all-cells!
  "
  Takes a transient vor and finishes all the break points.
  The function assumes that all of the events have been processed.
  "
  [vor]
  (loop [v vor breaks (seq (persistent! (:breaks vor)))]
    (let [{completed :completed
           y :scan} v]
      (if-not (empty? breaks)
        (let [{side :side
               begin :begin
               :as break} (first breaks)
              {m :m
               b :b
               is-vertical :is-vertical
               :as edge} (:edge break)
              end (if is-vertical
                    (point/->Point Infinity (:x (:begin break)))
                    (if (> m 0)
                      (condp = side
                        :left (point/->Point -Infinity -Infinity)
                        :right (point/->Point Infinity Infinity))
                      (condp = side
                        :left (point/->Point -Infinity Infinity)
                        :right (point/->Point Infinity -Infinity))))
              complete (edge/new-complete break end)
              completed (conj! completed complete)]
          (recur (assoc! v :completed completed)
                 (rest breaks)))
        (dissoc! v :breaks)))))

(def x-is-infinite?
  (comp is-infinite? :x))

(defn polygon-for-cells [site vor]
  (let [cell-indices (get (:cells vor) site)
        edges (:completed vor)
        m (->> cell-indices
               (map (comp
                     (fn [edge]
                       (if (= site (:left edge))
                         [(:p0 edge) (:p1 edge)]
                         [(:p1 edge) (:p0 edge)]))
                     (fn [n]
                       (nth edges n))))
               (remove #(= (first %) (second %)))
               (flatten)
               (apply hash-map))
        inv (clojure.set/map-invert m)
        k (if-let [k (some #(when (not (contains? inv %)) %) (keys m))]
            k
            (key (first m)))
        d (loop [l [k]]
            (let [n (get m (peek l))]
              (if (or (= n (first l))
                      (nil? n))
                l
                (recur (conj l n)))))
        ]
    (if (not= (count d) (count m))
      ;; decide whether we add one edge or two,
      ;; add 'em
      ;; (println (count d) (count m) "\n" m "\n" d)
      nil
      )
    d))

;; for each cell, we'll go through and clip any edges which need to be clipped
;; the problem with this is an edge may be shared and need to get clipped
;; maybe we can do this as a breadth first search algorithm

;; TODO: refactor this to be less gross

(defn clip-left-out
  "
  Clips the completed edge to the extent boundary
  For now defer adding edges
  "
  [[xmin xmax ymin ymax]
   {{m :m b :b} :edge
    p0 :p0
    p1 :p1
    :as c}]
  ;; we want to find out what it intercepts
  (cond
    (> (:x p0) (:x p1))
    (let [clip-y (+ (* m xmax) b)
          [x y] (cond
                  (> clip-y ymax) [(/ (- ymax b) m) ymax]
                  (< clip-y ymin) [(/ (- ymin b) m) ymin]
                  :else [xmax clip-y])]
      (assoc c :p0 (point/->Point x y)))
    (< (:x p0) (:x p1))
    (let [clip-y (+ (* m xmin) b)
          [x y] (cond
                  (> clip-y ymax) [(/ (- ymax b) m) ymax]
                  (< clip-y ymin) [(/ (- ymin b) m) ymin]
                  :else [xmin clip-y])]
      (assoc c :p0 (point/->Point x y)))))

(defn clip-right-out
  "
  Clips the completed edge to the extent boundary
  For now defer adding edges
  "
  [[xmin xmax ymin ymax]
   {{m :m b :b} :edge
    p0 :p0
    p1 :p1
    :as c}]
  ;; we want to find out what it intercepts
  (cond
    (> (:x p1) (:x p0))
    (let [clip-y (+ (* m xmax) b)
          [x y] (cond
                  (> clip-y ymax) [(/ (- ymax b) m) ymax]
                  (< clip-y ymin) [(/ (- ymin b) m) ymin]
                  :else [xmax clip-y])]
      (assoc c :p1 (point/->Point x y)))
    (< (:x p1) (:x p0))
    (let [clip-y (+ (* m xmin) b)
          [x y] (cond
                  (> clip-y ymax) [(/ (- ymax b) m) ymax]
                  (< clip-y ymin) [(/ (- ymin b) m) ymin]
                  :else [xmin clip-y])]
      (assoc c :p1 (point/->Point x y)))))

(defn clip-cells [{cells :cells
                   completed :completed
                   extent :extent
                   :as vor}]
  (if (nil? extent)
    vor
    (->> completed
         (map (fn [{p0 :p0 p1 :p1 :as c}]
                (let [out0 (outside-extent? extent p0)
                      out1 (outside-extent? extent p1)]
                  (cond
                    (and out0 (not out1)) (clip-left-out extent c)
                    (and out1 (not out0)) (clip-right-out extent c)
                    (and out0 out1) nil
                    :else c))))
         (remove nil?)
         (into []))))

(defn polygons [vor]
  (if-some [cells (:cells vor)]
    (map #(polygon-for-cells % vor) (keys cells))
    nil))

(defn finish [vor]
  (let [v (-> vor
              (make-transient)
              (process-all-events!)
              (complete-all-cells!)
              (make-persistent!))
        v (as-> v v
              (assoc v :completed (clip-cells v))
              (assoc v :cells (make-cells v)))]

    (assoc v :cells (make-cells v))))
