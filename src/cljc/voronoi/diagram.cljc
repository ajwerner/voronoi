(ns voronoi.diagram
  (:require [voronoi.point :as point]
            [voronoi.edge :as edge]
            [voronoi.builder :as builder]
            [voronoi.clip :as clip]
            [voronoi.util :refer [Infinity -Infinity]]))

(def ^:private arc-break (comp :right first))

(defn get-breaks
  "returns the break points from a app builder"
  [vor]
  (->> (seq (:arcs vor))
       (map arc-break)
       (remove nil?)))

(defn ^:private complete-all-breaks!
  "
  Takes a transient vor and finishes all the break points.
  The function assumes that all of the events have been processed.
  "
  [vor]
  (loop [v vor breaks (get-breaks vor)]
    (let [{edges :edges} v]
      (if (empty? breaks)
        v
        (let [{side :side :as break} (first breaks)
              {m :m b :b is-vertical :is-vertical} (:edge break)
              end (if is-vertical
                    (do
                      (condp side =
                        :right (point/->Point (:x (:begin break)) Infinity)
                        :vert (point/->Point (:x (:begin break)) Infinity)
                        :left (point/->Point (:x (:begin break)) -Infinity)))
                    (cond
                      (> m 0) (condp = side
                                :left (point/->Point -Infinity -Infinity)
                                :right (point/->Point Infinity Infinity))
                      (< m 0) (condp = side
                                :left (point/->Point -Infinity Infinity)
                                :right (point/->Point Infinity -Infinity))
                      (= m 0) (condp = side
                                :left (point/->Point -Infinity b)
                                :right (point/->Point Infinity b))))
              complete (edge/new-complete break end)
              edges (conj! edges complete)]
          (recur (assoc! v :edges edges) (rest breaks)))))))

(defn ^:private add-site-edge
  [edge-list half-edge]
  (if edge-list
    (conj edge-list half-edge)
    [half-edge]))

(defn ^:private add-cell-edges
  [site-edges {half-edge :edge} i]
  (-> site-edges
      (update (:site1 half-edge) add-site-edge i)
      (update (:site2 half-edge) add-site-edge i)))

(defn ^:private make-cells
  [vor]
  (loop [i 0
         edges (seq (:edges vor))
         cells {}]
    (if-let [he (first edges)]
      (recur (+ i 1)
             (rest edges)
             (add-cell-edges cells he i))
      cells)))

(defn ^:private cell-to-polygon [site cell edges]
  (->> cell
       (map #(nth edges %))
       (remove #(= (:p0 %) (:p1 %)))
       (map (fn [edge]
              (if (= site (:left edge))
                (:p0 edge)
                (:p1 edge))))))

(defn polygon-from-cell
  [edges site cell]
  {:cell (cell-to-polygon site cell edges)
   :site site})

(defn polygons [{extent :extent edges :edges cells :cells}]
  (if extent
    (map #(apply polygon-from-cell edges %) cells)))

(defn ^:private build-cells
  [vor]
  (as-> vor v
        (assoc v :edges (clip/clip-edges v))
        (assoc v :cells (make-cells v))
        (clip/order-cells v)))

(defn finish
  "finish-builder takes a app builder, process all events, and clips cells to the extent
  as appropriate."
  [vor]
  (-> vor
      (builder/make-transient)
      (builder/process-all-events!)
      (complete-all-breaks!)
      (builder/scan-by! 10)
      (builder/make-persistent!)
      (build-cells)
      (dissoc :arcs)
      (dissoc :scan)))
