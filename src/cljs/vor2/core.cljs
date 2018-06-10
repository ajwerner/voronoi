(ns vor2.core
  (:require [reagent.core :as reagent :refer [atom]]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]
              [quil.core :as q :include-macros true]
              [quil.middleware]
              [voronoi.voronoi :as vor]
              [voronoi.draw :refer [draw draw-f canvas-size]]
              [voronoi.components :refer [app-thing]]
              [voronoi.control :refer [mouse-pressed!
                                       reset-state!]]
              [voronoi.points :as p]))

;; -------------------------
;; Views

(defonce app-state (atom {}))
(def initial-points (p/random-points 100))
;;(reset-state! app-state initial-points)

(defn setup []
  (q/frame-rate 60)
  (q/background 250)
  app-state)

(defn home-page []
  [:div [:h2 "Welcome to vor2"]
   [app-thing app-state]
   [:div [:a {:href "/about"} "go to about page"]]])

(defn about-page []
  [:div [:h2 "About vor2"]
   [:div [:a {:href "/"} "go to the home page"]]])

;; -------------------------
;; Routes

(defonce page (atom #'home-page))

(defn current-page []
  [:div [@page]])

(secretary/defroute "/" []
  (reset! page #'home-page))

(secretary/defroute "/about" []
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
