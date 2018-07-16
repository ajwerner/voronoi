(ns app.components.events-panel
  (:require [voronoi.voronoi :as vor]
            [voronoi.event :as event]))

(defn event-row [ev]
  (let [p (str "("
               (.toFixed (:x ev) 2)
               ","
               (.toFixed (:y ev) 2)
               ")")]
    ^{:key ev}
    [:tr
     [:td p]
     [:td (if (instance? event/CircleEvent ev)
            "Circle"
            "Site")]]))

(defn events-panel [vor-cursor]
  (fn []
    [:div.events-panel
     {}
     [:table
      [:thead
       [:tr
        [:th "Point"]
        [:th "Type"]]]
      [:tbody
       (map event-row (:events @vor-cursor))]]]))
