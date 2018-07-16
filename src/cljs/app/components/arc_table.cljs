(ns app.components.arc-table
  (:require [reagent.core :as reagent :refer [atom]]
            [voronoi.arc :as arc]
            [voronoi.util :as u]
            [voronoi.point :refer [distance]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arc Table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn point [p]
  (str "("
       (.toFixed (:x p) 2)
       ","
       (.toFixed (:y p) 2)
       ")"))


(defn arc-table
  "arc-table isn't that interesting.
  It creates a really ugly table of the wavefront.
  It was an early experiment with a reagent component."
  [vor]
  (fn []
    [:div
     [:table
      [:thead
       [:tr
        [:th "Left"]
        [:th "Right"]
        [:th "Length"]
        [:th "Site"]
        [:th "BreakLeft"]
        [:th "BreakRight"]]]
      [:tbody
       (let [{:keys [arcs scan]} @vor]
         (for [[arc _] arcs]
           (let [[l r] (arc/arc-points arc scan)
                 len (distance l r)]
             ^{:key arc}
             [:tr
              [:td [point l] ]
              [:td [point r] ]
              [:td (str len)]
              [:td (str (:point arc))]
              [:td (str "(" (:left (:left arc)) ", " (:right (:left arc)) ")") ]
              [:td (str "(" (:left (:right arc)) ", " (:right (:right arc)) ")") ]])))
       ]]]))

(defn arc-table-and-toggle [vor]
  (let [show-table (atom false)
        toggle-show-table #(swap! show-table not)]
    (fn []
      [:div
       [:div "show table"
        [:input
         {:type "checkbox"
          :onChange toggle-show-table
          :id "showTableToggle"
          :checked @show-table}]]
       (if @show-table
         [arc-table vor])])))