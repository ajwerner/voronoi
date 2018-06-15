(ns voronoi.components
  (:require [reagent.core :as reagent :refer [atom]]
            [voronoi.voronoi :as vor]
            [voronoi.points :as points]

            [voronoi.components.arc-table :refer [arc-table-and-toggle]]
            [voronoi.components.svg :refer [voronoi-svg]]
            [voronoi.components.control-panel :refer [control-panel]]))

(def initial-points points/some-cool-stuff)

(enable-console-print!)
(defn new-app-thing [db id]
  (swap! db #(if % % {:voronoi (vor/new-voronoi initial-points)
                      :id id}))
  (let [vor (reagent/cursor db [:voronoi])
        width (reagent/atom {:active false
                             :width 100
                             })
        md (fn [name]
             (fn [ev]
               ;; (println  name (let [rect (.getBoundingClientRect (.-target ev))]
               ;;                           [(- (.-clientX ev) (.-left rect))
               ;;                            (- (.-clientY ev) (.-top rect))])
             ))

        pe (fn [ev]
             (swap! width identity ))
             ]
    (fn []
      [:section.voronoi-widget {:id id}
       [:div.graphics
        {:style {:width (str @width "%")
                 :position "fixed"
                 :overflow "hidden"}
         :on-touch-end (md :te)
         :on-touch-start (md :ts)
         :on-touch-move (md :tm)
         :on-touch-cancel (md :tc)
         :on-drag (md :d)
         :on-drag-over (md :do)
         :on-mouse-up (md :mu)
         :on-mouse-down (md :md)
         :on-mouse-over (md :mo)}
        [voronoi-svg vor]]
       [control-panel db]
       [arc-table-and-toggle vor]])))

(defn animation-playground [db]
  (let []
    (fn []
      [:div ;;[:h2 "Voronoi diagrams"]
       ;; [:div
       ;;  [:p "Just a pretty picture for now"]]
       [new-app-thing db "animation-playground"]
       [:div [:a {:href "/#intro"} "<- back"]]])))

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
   [:div [:a {:href "/#animation-playground"} "voronoi diagrams ->"]]])
