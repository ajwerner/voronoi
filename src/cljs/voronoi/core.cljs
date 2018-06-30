(ns voronoi.core
  (:require [clojure.string :as str]
            [reagent.core :as reagent :refer [atom]]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [voronoi.voronoi :as vor]
            [voronoi.control :refer [reset-state!]]
            [voronoi.points :as p]
            [voronoi.components :as components]
            [voronoi.routes :as routes]))

;; -------------------------
;; State

(defonce app-state (atom {:animation-page nil
                          :misc nil
                          :map-page {:outline nil
                                     :data nil}}))

(secretary/set-config! :prefix "#")

;; -------------------------
;; Views
;; -------------------------

(defonce page (atom #'components/intro))

(defn current-page []
  [:div [@page]])

(defonce map-page-cursor
  (reagent/cursor app-state [:map-page :states]))

(defonce map-data-cursor
  (reagent/cursor app-state [:map-page :data]))

(components/get-map map-page-cursor)
(components/get-data map-data-cursor)

(defonce animation-playground-page
  #(components/animation-playground
    (reagent/cursor app-state [:animation-page])))

(defonce misc-page
  #(components/misc
    (reagent/cursor app-state [:misc])))

(defonce map-page
  #(components/map-thing map-page-cursor map-data-cursor))

(secretary/defroute map-p #"/map" []
  (reset! page #'map-page))

(secretary/defroute intro-p #"/(intro)?" []
  (reset! page #'components/intro))

(secretary/defroute misc-p "/misc" []
  (reset! page #'misc-page))

(secretary/defroute animation-page "/animation-playground" []
  (reset! page #'animation-playground-page))

(secretary/defroute voronoi-description "/voronoi-diagrams" []
  (reset! page #'components/voronoi-diagrams))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (let [hash-index (str/index-of path \#)
             path (subs path (+ hash-index 1))]
         (secretary/dispatch! path)))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))

(defn ^:export main [] (init!))
