(ns voronoi.components
  (:require [reagent.core :as reagent :refer [atom]]
            [voronoi.voronoi :as vor]
            [voronoi.points :as points]
            [voronoi.components.arc-table :refer [arc-table-and-toggle]]
            [voronoi.components.svg :refer [voronoi-svg]]
            [voronoi.components.events-panel :refer [events-panel]]
            [voronoi.components.control-panel :refer [control-panel]]))

(defonce initial-points points/some-cool-stuff)

(enable-console-print!)
(defn new-app-thing [db id]
  (swap! db #(if % % {:voronoi (vor/new-voronoi initial-points)
                      :id id
                      :scroll nil}))
  (let [vor (reagent/cursor db [:voronoi])
        scroll (reagent/cursor db [:scroll])]
    (fn []
      [:section.voronoi-widget {:id id}
       [:div.graphics
        {:style {:position "fixed"
                 :overflow "hidden"}}
        [voronoi-svg vor scroll]]
       [control-panel db]
       [arc-table-and-toggle vor]
      ;; [events-panel vor]
       ])))

(defn animation-playground [db]
  (let []
    (fn []
      [:div [:h2 "Voronoi diagrams"]
       [:div [:p "Just a pretty picture for now"]]
       [new-app-thing db "animation-playground"]
       [:div [:a {:href "#/intro"} "<- back"]]])))

(def new-misc-state
  {:crazy (vor/finish (vor/new-voronoi initial-points))
   :random (vor/finish (vor/new-voronoi (points/random-points 500)))
   :circle (vor/finish (vor/new-voronoi (points/circle-points 71 200 100 100)))
   })

;;:grid (vor/finish (vor/new-voronoi (points/extent-grid [0 1000 0 1000] 100)))
;;[:div [voronoi-svg (reagent/cursor db [:grid])]]

(defn misc [db]
  (swap! db #(if % % new-misc-state))
  (let []
    (fn []
      [:div
       [:div
        [:div [voronoi-svg (reagent/cursor db [:random])]]
        [:div [voronoi-svg (reagent/cursor db [:crazy])]]
        [:div [voronoi-svg (reagent/cursor db [:circle])]]

        [:div.links
         [:a {:href "#/intro"} "Tell me more ->"]]]])))

(defn intro []
  [:div [:h2 "Voronoi Diagrams"]
   [:div
    [:div
     [:h3 "What is this?"]
     [:p
      "This post is primarily about Voronoi diagram"
      " but along the way it's also about:"]
     [:ul (map #(into ^{:key %} [:li] %)
               ["Clojure/Clojurescript"
                "React/Reagent and Single Page applications"
                "Drawing in the browser (Processing/Quil and SVGs)"
                "Robust geometric predicates with floating point"])]
     [:h3 "What is this not?"]
     [:p "Novel, this project has no novel contributions to offer to the world."
      " Any seemingly deep insight was much more deeply pursued by somebody else."
      " I'll try to point references to some things which I glanced at but often gave up on understanding completely for the moment."]]]
   [:div
    [:a {:href "#/animation-playground"} "voronoi diagrams ->"]
    [:br]
    [:a {:href "#/misc"} "misc <-"]
    [:br]
    [:a {:href "#/voronoi-diagrams"} "about diagrams"]]])

(defn voronoi-diagrams []
  [:div
   [:h2 "What's a Voronoi Diagram"]
   [:div
    [:p
     "Say you have a bunch of points in plane. "
     "Maybe these bare cities on a map*"
     "And you want to know the area closest to each city based on distance."
     "Voronoi diagrams are your tool"]
    [:p
     "A common use case for these diagrams are to create tooltips for graphs"
     "Efficient algorithms exist to find which polygon in a set contain some point"
     "(See point containment search, these are actually easy to make much faster when you can ensure that the polygons do not overlap)"
     "See BSP trees"]]])
