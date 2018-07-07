(ns voronoi.components
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            [voronoi.voronoi :as vor]
            [voronoi.points :as points]
            [voronoi.components.arc-table :refer [arc-table-and-toggle]]
            [voronoi.components.svg :refer [voronoi-svg interactive-svg voronoi-group]]
            [voronoi.components.events-panel :refer [events-panel]]
            [voronoi.components.control-panel :refer [control-panel]]
            [voronoi.components.examples :as examples]
            [re-com.core :as rc]))

(defonce initial-points points/some-cool-stuff)

(defn new-app-thing [db id]
  (swap! db #(if % % {:voronoi (vor/new-voronoi initial-points)
                      :id      id
                      :scroll  nil}))
  (let [vor (reagent/cursor db [:voronoi])
        scroll (reagent/cursor db [:scroll])]
    (fn []
      [:section.voronoi-widget {:id id}
       [:div.graphics
        {:style {:position "fixed"
                 :overflow "hidden"}}
        [interactive-svg vor scroll]]
       [control-panel db]
       [arc-table-and-toggle vor]
       ])))

(defn animation-playground [db]
  (let []
    (fn []
      [:div [:h2 "Voronoi diagrams"]
       [:div [:p "Just a pretty picture for now"]]
       [new-app-thing db "animation-playground"]
       [:div [:a {:href "#/intro"} "<- back"]]])))

(defn misc [db]
  (let []
    (fn []
      [rc/v-box
       :children
       [[rc/box
         :height "95vh"
         :child [examples/examples]]
        [rc/h-box
         :children
         [[:a {:href "#/map"} "<- Map"]
          [rc/gap :size "1"]
          [:a {:href "#/intro"} "Tell me more ->"]]]]])))

(defn bulleted-list [& li-text-items]
  [:ul (map #(into ^{:key %} [:li] %) li-text-items)])

(defn intro []
  [:div [:h2 "Voronoi Diagrams"]
   [:div
    [:div
     [:h3 "What is this?"]
     [:p "This post is primarily about Voronoi diagram but along the way it's also about:"]
     [bulleted-list
      "Clojure/Clojurescript"
      "React/Reagent and Single Page applications"
      "Drawing in the browser (Processing/Quil and SVGs)"
      "Robust geometric predicates with floating point"]
     [:h3 "What is this not?"]
     [:p "Novel, this project has no novel contributions to offer to the world."
      " Any seemingly deep insight was much more deeply pursued by somebody else."
      " I'll try to point references to some things which I glanced at but often gave up on understanding completely for the moment."]]]
   [:div
    [:a {:href "#/misc"} "<- Examples"]
    [:br]
    [:a {:href "#/animation-playground"} "Interactive playground ->"]
    [:br]
    [:a {:href "#/voronoi-diagrams"} "About Diagrams ->"]]])

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
     "See BSP trees"]
    [:div
     [:a {:href "#/intro"} "<- Intro"]]]])
