(ns app.components.examples
  (:require-macros [cljs.core.async :refer [go]])
  (:require [voronoi.points :as points]
            [re-frame.core :as rf]
            [voronoi.voronoi :as vor]
            [app.components.svg :as svg]
            [re-com.core :as rc]))

(def misc-state
  [{:id     :crazy
    :points points/some-cool-stuff}
   {:id     :random
    :points (points/random-points 2000)}
   {:id     :circle
    :points (points/circle-points 71 200 100 100)}
   {:id     :circle2
    :points (points/circle-points 53 200 100 100)}
   {:id     :only-2
    :points [{:x 100 :y 100} {:x 200 :y 110}]
    :extent [70 220 90 120]}
   {:id     :only-2-1
    :points [{:x 100 :y 100} {:x 200 :y 100}]
    :extent [70 220 90 120]}
   {:id     :only-2-2
    :points [{:x 200 :y 200} {:x 200 :y 100}]
    :extent [70 220 90 300]}
   {:id     :only-2-3
    :points [{:x 200 :y 201} {:x 200 :y 100}]
    :extent [70 220 90 300]}
   {:id     :clip-bottom
    :points [{:x 210 :y 300} {:x 300 :y 100} {:x 49 :y 21}]}
   {:id     :clip-right
    :points [{:x 200 :y 180} {:x 300 :y 100} {:x 49 :y 21}]}
   {:id     :clip-top
    :points [{:x 200 :y 180} {:x 300 :y 100} {:x 251 :y 25}]}
   {:id     :clip-left
    :points [{:x 10 :y 180} {:x 300 :y 100} {:x 251 :y 40}]}
   {:id     :clip-left-straight
    :points [{:x 50 :y 25} {:x 50 :y 50} {:x 100 :y 75} {:x 100 :y 25}]}
   {:id     :clip-up-straight
    :points [{:x 50 :y 25} {:x 50 :y 75} {:x 100 :y 75} {:x 100 :y 25}]}
   {:id     :clip-up-right
    :points [{:x 50 :y 25} {:x 79 :y 100} {:x 100 :y 25}]}
   {:id     :clip-up-left
    :points [{:x 50 :y 25} {:x 73 :y 100} {:x 100 :y 25}]}
   {:id     :clip-up-middle
    :points [{:x 50 :y 25} {:x 75 :y 100} {:x 100 :y 25}]}
   {:id     :grid
    :points (points/grid 20 [0 0] 20 20)}])

(rf/reg-event-fx
  ::initialize
  (fn [{db :db} _]
    {:db db
     ::initialize nil}))

(rf/reg-fx
  ::initialize
  (fn [_ _]
    (doall
      (for [d misc-state]
        (let []
          (rf/dispatch [::ev-load-example d]))))))

(rf/reg-event-fx
  ::ev-load-example
  (fn [{db :db} [_ d]]
    {:db db
     ::load-test-data d}))

(rf/reg-event-db
  ::loaded-test-data
  (fn [db [_ id vor]]
    (update db ::test-data
            #(if % (assoc % id vor) {id vor}))))

(rf/reg-fx
  ::load-test-data
  (fn [{id :id points :points extent :extent :as data} _]
    (go
      (rf/dispatch [::loaded-test-data id (vor/new-voronoi points :extent extent)]))))

(rf/reg-sub
  ::example-data-keys
  (fn [db _]
    (keys (::test-data db))))

(rf/reg-sub
  ::test-data
  (fn [db _]
    (::test-data db)))

(rf/reg-sub
  ::tab-vor
  :<- [::test-data]
  :<- [::cur-tab]
  (fn [[d t] _]
    (t d)))

(defn tab-svg []
  (fn []
    [rc/box
     :align :center
     :size "1"
     :max-height "93vmin"
     :child [svg/voronoi-svg (rf/subscribe [::tab-vor])]]))

(rf/reg-sub
  ::tabs
  :<- [::example-data-keys]
  (fn [keys _]
    (into [] (for [k keys] {:id k :label (name k)}))))

(rf/reg-sub
  ::cur-tab
  (fn [db _]
    (if-let [id (::cur-tab db)]
      id
      (first (keys (::test-data db))))))

(rf/reg-event-db
  ::set-cur-tab
  (fn [db [_ id]]
    (assoc db ::cur-tab id)))

(defn examples-controller []
  [rc/box
   :width "150px"
   :max-height "94vh"
   :justify :center
   :style {:overflow "auto"}
   :child
   [rc/vertical-bar-tabs
    :tabs (rf/subscribe [::tabs])
    :model (rf/subscribe [::cur-tab])
    :on-change #(rf/dispatch [::set-cur-tab %])]])

(defn examples-page []
  (fn []
    [rc/h-box
     :size "auto"
     :children [[tab-svg]
                [examples-controller]]]))
