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
  (swap! db #(if % % {:voronoi (vor/new-voronoi-builder initial-points)
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
       [:div [:a {:href "#/examples"} "<- Examples"]]])))


(defn bulleted-list [& li-text-items]
  [:ul (map #(into ^{:key %} [:li] %) li-text-items)])


(defn about-diagrams []
  [rc/v-box
   :align :center
   :children
   [[rc/title :level :level1 :label "What's a Voronoi Diagram?"]
    [rc/p
     "Say you have a bunch of points in plane.
      Maybe these bare cities on a map*
      And you want to know the area closest to each city based on distance.
      Voronoi diagrams are your tool"]
    [rc/p
     "A common use case for these diagrams are to create tooltips for graphs.
     Efficient algorithms exist to find which polygon in a set contain some point
     (See point containment search, these are actually easy to make much faster when you can ensure that the polygons do not overlap).
     See BSP trees."]
    [rc/p
     "The goals of the diagram is to split the plane in to non-overlapping convex polygons which each
     The basic principle here is that the cells represent the set of points which are closest
     to the point contained by them. In other words, the interior of a cell represents the set of points
     the minimum distance to that points. That means the boundaries of cells are the set of points which have an equal
     minimum distance. So it turns out that if we are running algorithm as a sweepline, then we have parabolas.
     "]]])

(defn link [link-info text-fn]
  (println link-info)
  (if link-info
    [rc/box
     :child [:a {:href (:href link-info)}
             (text-fn link-info)]]))



(defn links [prev next]
  (let [prev-text (fn [{:keys [text]}]
                    (str "<- " text))
        next-text (fn [{:keys [text]}]
                    (str text " ->"))
        children (->> [(link prev prev-text)
                       [rc/gap :size "1"]
                       (link next next-text)]
                      (remove nil?)
                      (into []))
        ]
    (println "children " prev next " adsfadsf "
             [(link prev prev-text)
              [rc/gap :size "1"]
              (link next next-text)])
    [rc/h-box
     :justify :start
     :width "100%"
     :height "100%"
     :children children]))

(defn page [content & {:keys [prev next]}]
  [rc/v-box
   :align :center
   :justify :between
   :children
   [[rc/box
     :height "95vh"
     :width "100%"
     :style {:overflow "auto"}
     :child [content]]
    [links prev next]]])

(defn map-page []
  [page voronoi.components.us-map/map-thing
   :next {:text "Intro" :href "#/intro"}])

(defn voronoi-diagrams []
  [page  about-diagrams
   :prev {:text "Intro" :href "#/intro"}
   :next {:text "Examples" :href "#/examples"}])

(defn examples-page []
  [page examples/examples-page
   :prev {:text "About Voronoi Diagrams" :href "#/voronoi-diagrams"}
   :next {:text "Animations" :href "#/animation-playground"}])

(defn intro []
  [rc/box
   :align :center
   :child
   [rc/v-box
    :children
    [[rc/title :level :level1 :label "Voronoi Diagarms"]
     [rc/title :level :level3 :label "What is this?"]
     [rc/p
      "This post is primarily about Voronoi diagram but along the way it's also about:"
      [bulleted-list
       "Clojure/Clojurescript"
       "React/Reagent and Single Page applications"
       "Drawing in the browser (Processing/Quil and SVGs)"
       "Robust geometric predicates with floating point"]]
     [rc/title :level :level3 :label "What is this not?"]
     [rc/p
      "Novel, this project has no novel contributions to offer to the world.
       Any seemingly deep insight was much more deeply pursued by somebody else.
       I'll try to point references to some things which I glanced at but often gave up on understanding completely for the moment."]]]])

(defn intro-page []
  [page intro
   :prev {:text "Contrived example" :href "#/map"}
   :next {:text "About Voronoi Diagrams" :href "#/voronoi-diagrams"}])

