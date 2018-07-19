(ns app.slides.subs
  (:require [re-frame.core :as rf]
            [voronoi.core :as vor]
            [app.slides.db :as db]
            [app.svg :as svg]))

(rf/reg-sub
  :slides/builder
  (fn [db _]
    (:slides/builder db)))

(rf/reg-sub
  :builder/running?
  (fn [db [_ id]]
    (some? (get-in db [:play-builder id]))))

(rf/reg-sub
 :builder/has-prev?
 (fn [db [_ id]]
   (some? (db/get-prevs db id))))

(rf/reg-sub
 :builder/has-next?
 (fn [[_ id] _]
   (rf/subscribe id))
 (fn [vor _]
   (some? (first (:events vor)))))

(rf/reg-sub
 :builder/can-next?
 (fn [[_ id] _]
   [(rf/subscribe [:builder/running? id])
    (rf/subscribe [:builder/has-next? id])])
 (fn [[running? has-next?] _]
   (and has-next? (not running?))))

(rf/reg-sub
 :builder/can-prev?
 (fn [[_ id] _]
   [(rf/subscribe [:builder/running? id])
    (rf/subscribe [:builder/has-prev? id])])
 (fn [[running? has-prev?] [_ id]]
   (and has-prev? (not running?))))

(rf/reg-sub
  :slides/view-box
  (fn [[_ id]]
    (rf/subscribe id))
  (fn [vor _]
    (svg/view-box-extent (:extent vor) 0)))
