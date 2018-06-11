(ns vor2.core
  (:require [reagent.core :as reagent :refer [atom]]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [voronoi.voronoi :as vor]
            [voronoi.draw :refer [draw draw-f canvas-size]]
            [voronoi.components :refer [app-thing]]
            [voronoi.control :refer [
                                     reset-state!]]
            [voronoi.points :as p]))

;; -------------------------
;; State

(defonce app-state (atom {}))
(def initial-points p/some-cool-stuff)
(reset-state! app-state initial-points)

;; -------------------------
;; Views


(defn home-page []
  [:div [:h2 "Voronoi Diagrams"]
   [:div
    [:div
     [:h3 "What is this?"]
     [:p
      "This post is primarily about Voronoi diagram"
      " but along the way it's also about:"
      [:ul (map #(into ^{:key %} [:li] %) ["Clojure/Clojurescript"
                                 "React/Reagent and Single Page applications"
                                 "Drawing in the browser (Processing/Quil and SVGs)"
                                 "Robust geometric predicates with floating point"])]

]
     [:h3 "What is this not?"]
     [:p "Novel, this project has no novel contributions to offer to the world."
      " Any seemingly deep insight was much more deeply pursued by somebody else."
      " I'll try to point references to some things which I glanced at but often gave up on understanding completely for the moment."]
     ]

    ]
   [:div [:a {:href "/voronoi/interactive"} "voronoi diagrams ->"]]])

(defn about-page []
  [:div [:h2 "Voronoi diagrams"]
   [:div
    [:p "Just a pretty picture for now"] ]
   [app-thing app-state]
   [:div [:a {:href "/voronoi/"} "<- back"]]])

;; -------------------------
;; Routes

(defonce page (atom #'home-page))

(defn current-page []
  [:div [@page]])

(secretary/defroute "/voronoi/" []
  (reset! page #'home-page))

(secretary/defroute "/voronoi/interactive" []
  (reset! page #'about-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))

(defn ^:export main [] (init!))
