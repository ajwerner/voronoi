(ns voronoi.core
  (:require [clojure.string :as str]
            [reagent.core :as reagent :refer [atom]]
            [re-frame.core :as rf]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [voronoi.voronoi :as vor]
            [voronoi.control :refer [reset-state!]]
            [voronoi.points :as p]
            [voronoi.components :as components]
            [voronoi.components.us-map :as us-map]
            [voronoi.components.examples :as examples]))

;; -------------------------
;; Routing

(secretary/defroute map-p #"/(map)?" []
  (rf/dispatch [:page :map]))

(secretary/defroute intro-p #"/intro" []
  (rf/dispatch [:page :intro]))

(secretary/defroute examples-p "/examples" []
  (rf/dispatch [:page :tests]))

(secretary/defroute animation-page "/animation-playground" []
  (rf/dispatch [:page :playground]))

(secretary/defroute voronoi-description "/voronoi-diagrams" []
  (rf/dispatch [:page :voronoi-diagrams-intro]))


;; -------------------------
;; State

(defonce app-state (atom {:animation-page nil
                          :examples nil
                          :map-page {:outline nil
                                     :data nil}}))

;; -------------------------
;; Event handlers

(rf/reg-event-fx
 :initialize
 (fn [db  _]
   db))


(rf/reg-event-db
 :page
 (fn [db [_ new-page]]
   (assoc db :page new-page)))

(rf/reg-sub
 :current-page
 (fn [db _] (:page db)))

(secretary/set-config! :prefix "#")

;; -------------------------
;; Views
;; -------------------------

(reagent/cursor app-state [:animation-page])

(defonce examples-page #(components/examples-page))

(defonce map-page #(components/map-page))

(defonce animation-playground-page
  #(components/animation-playground
    (reagent/cursor app-state [:animation-page])))

(def routes
  {:intro #'components/intro-page
   :voronoi-diagrams-intro #'components/voronoi-diagrams
   :tests #'examples-page
   :playground #'animation-playground-page
   :map #'map-page})

(defn ui []
  [:div.container
   (if-let [p @(rf/subscribe [:current-page])]
     [(p routes)])])

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [ui] (.getElementById js/document "app")))

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
  (rf/dispatch [:initialize])
  (rf/dispatch [::us-map/get-city-data])
  (rf/dispatch [::us-map/get-map-data])
  (rf/dispatch [::examples/initialize])
  (mount-root))

(defn ^:export main [] (init!))
