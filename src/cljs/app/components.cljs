(ns app.components
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            [voronoi.core :as vor]
            [voronoi.points :as points]
            [app.components.arc-table :refer [arc-table-and-toggle]]
            [app.components.events-panel :refer [events-panel]]
            [app.components.control-panel :refer [control-panel]]
            [app.examples.examples :as examples]
            [app.slides.events :as slides]
            [app.playground.events]
            [app.playground.subs]
            [app.slides.events]
            [app.slides.subs]
            [app.us-map.views :as us-map]
            [app.playground.views :as playground]
            [re-com.core :as rc]
            [re-com.util :as rc-util]))





(defn bulleted-list [& li-text-items]
  [:ul (map #(into ^{:key %} [:li] %) li-text-items)])

(defn link [link-info text-fn]
  (if link-info
    [rc/box
     :child [:a {:href (:href link-info)} (text-fn link-info)]]))

(defn links [prev next]
  (let [prev-text (fn [{:keys [text]}]
                    (str "<- " text))
        next-text (fn [{:keys [text]}]
                    (str text " ->"))
        children (->> [[rc/gap :size "10px"]
                       (link prev prev-text)
                       [rc/gap :size "1"]
                       (link next next-text)
                       [rc/gap :size "10px"]]
                      (remove nil?)
                      (into []))]
    [rc/h-box
     :justify :center
     :width "100%"
     :height "100%"
     :children children]))

(defn page [content & {:keys [prev next]}]
  [rc/v-box
   :align :center
   :justify :between
   :children
   [[rc/box
     :min-height "94vh"
     :size "auto"
     :width "100%"
     :style {:overflow "auto"}
     :child [content]]
    [links prev next]]])

(defn p [& children]
  (let [c1 (first children)
        [m children] (if (map? c1)
                       [c1 (rest children)]
                       [{} children])
        m (rc-util/deep-merge
            {:style {:max-width "98vw"
                     :min-width "300px"
                     :margin "auto"}}
            m)]
    (into [rc/p m] children)))

(defn intro []
  [rc/box
   :align :center
   :child
   [rc/v-box
    :max-width "450px"
    :style {:padding-left  "5px"
            :padding-right "5px"}
    :children
    [[rc/title :level :level1 :label "Voronoi Diagrams"]
     [rc/title :level :level2 :label "What is this?"]
     [p
      "This post is primarily about Voronoi diagram but along the way it's also about:"]
     [bulleted-list
      "Clojure/Clojurescript"
      "React/Reagent and Single Page applications"
      "Drawing in the browser (Processing/Quil and SVGs)"
      "Robust geometric predicates with floating point"]
     [rc/title :level :level3 :label "What is this not?"]
     [p
      "Novel, this project has no particularly novel contributions to offer to the world.
       Any seemingly deep insight was much more deeply pursued by somebody else.
       I'll try to point references to some things which I glanced at but often gave up on understanding completely for the moment."]
     [p
      " I  particularly want to highlight the work in"
      [:a {:href "https://github.com/d3/d3-app"}
       " D3 Voronoi "]
      "and the"
      [:a {:href "https://visionscarto.net/the-state-of-d3-app"}
       " wonderful writeup by Philippe Rivi\u00e8re"]
      ". Probably for all practical use cases, D3 will be a better choice for a variety of reasons.
      One could argue that much of the code in this project is just an ad-hoc re-implementation of
      functionality found in D3. This project was an outlet for me to improve my ability to express
      myself visually with code and to grapple with the challenges of building UI. I hope you enjoy it
      and find it to be valuable educational material."]
     [rc/title :level :level3 :label "Maybe interesting things"]
     [p
      "Despite not being ground-breaking, there are some things that this implementations sets out to do that hopefully
      make it a little interesting. We'll explore those a little bit on later pages."]
     [bulleted-list
      "Careful handling of co-circular and co-linear points"
      "Exposed algorithm state for pedagogical and artistic properties."
      ]
     [rc/title :level :level3 :label "What else?"]
     [p
      "There's a bunch of stuff still to do"]
     [bulleted-list
      "Better handling of very near points"
      "Some clipping things for edge cases where the top and bottom
      edges or right and stuff"
      ]]]])

(defn about-diagrams []
  [rc/v-box
   :align :center
   :children
   [[rc/title :level :level1 :label "What's a Voronoi Diagram?"]
    [p
     "Say you have a bunch of points in plane.
      Maybe these bare cities on a map*
      and you want to know the area closest to each city based on distance.
      Voronoi diagrams are your tool."]
    [rc/box
     :max-height "500px"
     :height "100%"
     :width "100%"
     :size "auto"
     :child
     [playground/new-app-thing [:slides/builder]]]
    [p
     "The goals of the diagram is to split the plane in to non-overlapping convex polygons which each
     The basic principle here is that the cells represent the set of points which are closest
     to the point contained by them. In other words, the interior of a cell represents the set of points
     the minimum distance to that points. That means the boundaries of cells are the set of points which have an equal
     minimum distance. So it turns out that if we are running algorithm as a sweepline, then we have parabolas.
     "]]])

(defn references-body []
  [rc/v-box
   :align :center
   :children
   [[rc/title :level :level2 :label "References"]
    [bulleted-list
     [[:a {:href "https://visionscarto.net/the-state-of-d3-app"}
       "A wonderful writeup by Philippe Rivi\u00e8re"]
      ]
     "D3 Voronoi"
     "D3 geo"
     "The census data source"
     "The map data"]]])
(defn references []
  [page references-body
   :prev {:text "Examples" :href "#/examples"}])


(defn intro-page []
  [page intro
   :prev {:text "Contrived example" :href "#/map"}
   :next {:text "About Voronoi Diagrams" :href "#/app-diagrams"}])

(defn map-page []
  [page us-map/map-thing
   :next {:text "Intro" :href "#/intro"}])

(defn voronoi-diagrams []
  [page  about-diagrams
   :prev {:text "Intro" :href "#/intro"}
   :next {:text "Examples" :href "#/examples"}])

(defn examples-page []
  [page examples/examples-page
   :prev {:text "About Voronoi Diagrams" :href "#/app-diagrams"}
   :next {:text "Animations" :href "#/animation-playground"}])

(defn animation-playground []
  [page (playground/new-app-thing [:playground/builder])
   :prev {:text "Examples" :href "#/examples"}
   :next {:text "References" :href "#/references"}])