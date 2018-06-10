(ns voronoi.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [quil.core :as q :include-macros true]
            [quil.middleware]
            [voronoi.voronoi :as vor]
            [voronoi.draw :refer [draw draw-f canvas-size]]
            [voronoi.components :refer [app-thing]]
            [voronoi.control :refer [mouse-pressed!
                                     reset-state!]]
            [voronoi.points :as p]
            [reagent.core :as r]
            [react-dom :as react-dom]
            [cljs.core.async :as async]))

(enable-console-print!)

(defonce app-state (atom {}))

(def initial-points p/some-cool-stuff)
(reset-state! app-state initial-points)




(defn ^:export main []
  (r/render-component [[:div [:p "ji"]]]
            (.getElementById js/document "app")))
