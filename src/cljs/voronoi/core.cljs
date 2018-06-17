(ns voronoi.core
  (:require [reagent.core :as reagent :refer [atom]]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [voronoi.voronoi :as vor]
            [voronoi.components :refer [intro animation-playground]]
            [voronoi.control :refer [reset-state!]]
            [voronoi.points :as p]))

;; -------------------------
;; State

(defonce app-state (atom {:animation-page nil}))
(defonce animation-playground-page
  #(animation-playground
    (reagent/cursor app-state [:animation-page])))

;; -------------------------
;; Views
;; -------------------------

;; Routes

(defonce page (atom #'intro))

(defn current-page []
  [:div [@page]])

(secretary/defroute "/" []
  (reset! page #'intro))

(secretary/defroute "/#intro" []
  (reset! page #'intro))

(secretary/defroute "/#animation-playground" []
  (reset! page #'animation-playground-page))

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
