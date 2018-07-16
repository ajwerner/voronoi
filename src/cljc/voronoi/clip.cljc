(ns voronoi.clip
  (:require [voronoi.point :as point]
            [voronoi.edge :as edge]
            [clojure.set]))

(defn extent-zone
  [[xmin xmax ymin ymax :as extent] {x :x y :y}]
  (if (some? extent)
    [(< x xmin) (> x xmax) (< y ymin) (> y ymax)]))

(defn outside-extent?
  [extent p]
  (if extent
    (some true? (extent-zone extent p))
    false))

(defn ^:private order-cell
  "
  Takes the edges corresponding to a site and puts them in counter-clockwise
  order
  "
  [vor site cell-indices]
  (let [[xmin xmax ymin ymax :as extent] (:extent vor)
        edges (:edges vor)
        ;; we map the completed half-edges for each site to a list of
        ;; indices
        points (->> cell-indices
                    ;; map the indices to the values
                    (map #(nth edges %))
                    ;; point the edge in the right direction

                    (map (fn [edge]
                           (if (= site (:left edge))
                             [(:p0 edge) (:p1 edge)]
                             [(:p1 edge) (:p0 edge)]))))

        ;; map first to second
        ;; flatten to a list of pairs
        forwards (->> points
                      (remove #(= (first %) (second %)))
                      (flatten)
                      (apply hash-map))
        ;; we want to find if there are any edges which are not connected
        ;; if so, we know that they cross the extent boundary or are infinite.
        backwards (clojure.set/map-invert forwards)
        ;; using backwards we can find the edge to which no other edge points
        start-key (some #(when (not (contains? backwards %)) %)
                        (keys forwards))
        by-index (->> points
                      (map-indexed (fn [i [p1 p2]]
                                     (if (= p1 p2) nil [p1 i])))
                      (remove nil?)
                      (flatten)
                      (apply hash-map))
        ;; otherwise, use use any key because they all connect start-key
        start-key (if start-key start-key
                                (if-let [f (first forwards)]
                                  (key f)
                                  nil))
        start-key (if start-key start-key (key (first forwards)))
        ;; hook up the edges in order
        ;; this could definitely be faster
        d (loop [l [start-key]]
            (let [n (get forwards (peek l))]
              (if-not (or (= n (first l))
                          (nil? n))
                (recur (conj l n))
                l)))
        cell (->> d
                  (map #(get by-index %))
                  (remove nil?)
                  (map #(nth cell-indices %))
                  (reverse))]
    (if (and extent
             (not= (count d) (count forwards)))
      ;; decide whether we add one edge or two,
      ;; add 'em
      ;; the deal is we need to see which edges the unlinked points cross
      (let [f (first d)
            l (peek d)
            is-it? (fn [{x :x y :y}]
                     {:is-xmin (= x xmin) :is-xmax (= x xmax)
                      :is-ymin (= y ymin) :is-ymax (= y ymax)})
            fi (is-it? f)
            li (is-it? l)
            new-edge (fn [begin end]
                       (edge/new-complete {:begin begin
                                           :edge nil
                                           :side :left
                                           :left site
                                           :right nil}
                                          end))
            add-one (fn []
                      (let [i (count (:edges vor))]
                        (-> vor
                            (update :edges conj (new-edge l f))
                            (update :cells assoc site (conj cell i)))))]
        (if (= fi li)
          ;; there's maybe cases where points are in corners which I need to deal with
          (add-one)
          (let [add-two (fn [corner]
                          (let [i1 (count (:edges vor))
                                new-edge1 (new-edge l corner)
                                cell (conj cell i1)
                                i2 (+ i1 1)
                                new-edge2 (new-edge corner f)
                                cell (conj cell i2)
                                ]
                            (-> vor
                                (update :edges conj new-edge1 new-edge2)
                                (update :cells assoc site cell))))
                add-three (fn [corner1 corner2]
                            (let [i1 (count (:edges vor))
                                  new-edge1 (new-edge l corner1)
                                  cell (conj cell i1)
                                  i2 (+ i1 1)
                                  new-edge2 (new-edge corner1 corner2)
                                  cell (conj cell i2)
                                  i3 (+ i2 1)
                                  new-edge3 (new-edge corner2 f)
                                  cell (conj cell i3)
                                  ]
                              (-> vor
                                  (update :edges conj new-edge1
                                          new-edge2 new-edge3)
                                  (update :cells assoc site cell))))]
            ;; there are off by one cases and there are off-by 2 cases
            (cond
              ;; corner cases
              (and (:is-xmin li) (:is-ymax li)
                   (:is-xmax fi) (:is-ymin fi))
              (add-two (point/->Point xmax ymax))
              (and (:is-xmax li) (:is-ymin li)
                   (:is-xmin fi) (:is-ymax fi))
              (add-two (point/->Point xmin ymin))
              ;; two-edge non-corner cases
              (and (:is-xmin li) (:is-ymin fi))
              (add-two (point/->Point xmin ymin))
              (and (:is-ymin li) (:is-xmax fi))
              (add-two (point/->Point xmax ymin))
              (and (:is-xmax li) (:is-ymax fi))
              (add-two (point/->Point xmax ymax))
              (and (:is-ymax li) (:is-xmin fi))
              (add-two (point/->Point xmin ymax))
              ;; three edge cases
              (and (:is-xmin li) (:is-xmax fi))
              (add-three (point/->Point xmin ymin)
                         (point/->Point xmax ymin))
              (and (:is-ymin li) (:is-ymax fi))
              (add-three (point/->Point xmax ymin)
                         (point/->Point xmax ymax))
              (and (:is-xmax li) (:is-xmin fi))
              (add-three (point/->Point xmax ymax)
                         (point/->Point xmin ymax))
              (and (:is-ymax li) (:is-ymin fi))
              (add-three (point/->Point xmin ymax)
                         (point/->Point xmin ymin))
              :else (do
                      ;; TODO revisit this case
                      (update vor :cells assoc site cell))))))
      (update vor :cells assoc site cell))))

(defn order-cells
  [vor]
  (reduce-kv order-cell vor (:cells vor)))

;; for each cell, we'll go through and clip any edges which need to be clipped
;; the problem with this is an edge may be shared and need to get clipped
;; maybe we can do this as a breadth first search algorithm

(defn ^:private clip-one-side
  "
  Clips the completed edge to the extent boundary
  For now defer adding edges
  "
  [{{m :m b :b} :edge :as c}
   [xmin xmax ymin ymax]
   is-left?]
  (let [pk (if is-left? :p0 :p1)
        ok (if is-left? :p1 :p0)
        {px :x :as p} (pk c)
        {ox :x} (ok c)
        x (cond
            (> px ox) xmax
            (= px ox) px
            :else     xmin)
        clip-y (if (= px x)
                 (if (> (:y p) ymax) ymax ymin)
                 (+ (* m x) b))
        [x y] (cond
                (> clip-y ymax) [(/ (- ymax b) m) ymax]
                (< clip-y ymin) [(/ (- ymin b) m) ymin]
                :else [x clip-y])]
    (assoc c pk (point/->Point x y))))

(defn ^:private clip-edge
  [extent {p0 :p0 p1 :p1 :as c}]
  (let [zone0 (extent-zone extent p0)
        zone1 (extent-zone extent p1)
        in0 (not-any? true? zone0)
        in1 (not-any? true? zone1)]
    (cond
      (and in0 in1) c
      (not= in0 in1) (clip-one-side c extent in1)
      (some true? ;; can't intersect'
            (map #(and %1 %2) zone0 zone1)) nil
      :else (-> c
                (clip-one-side extent true)
                (clip-one-side extent false)))))

(defn clip-edges
  [{edges :edges
    extent :extent}]
  (if extent
    (->> edges
         (map #(clip-edge extent %))
         (remove nil?)
         (into []))
    edges))


