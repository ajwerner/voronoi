(ns app.components.control-panel
  (:require [reagent.core :as reagent :refer [atom]]
            [voronoi.util :refer [Infinity -Infinity isNaN?]]
            [voronoi.points :as points]
            [app.control :refer [toggle-pause-scan!
                                 reset-state!
                                 do-step!
                                 do-set-to!
                                 do-set-by!
                                 add-points!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Form helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-number [v]
  (try
    (let [num (js/Number v)]
      num)
    (catch :default e
      nil)))

(defn get-value-num [element]
  (try
    (let [num (js/Number el.value)]
      num)
    (catch :default e
      js/Number.NaN)))

(defn get-form-num [element-id]
  (let [el (.getElementById js/document element-id)]
    (get-value-num el)))

(defn form-key-press [action]
  (fn [ev]
    (if (= ev.which 13)
      (do
        (.preventDefault ev)
        (action)))))

(defn on-submit-action[ev]
  (.preventDefault ev)
  false)

(def default-form-keypress
  (form-key-press (fn [])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: replace with [reagent-forms "0.5.42"]

(defn button [text action]
  [:button {:href "#" :onClick action} text])

(defn make-text-input [id label action]
  [:div
   [:label {:for id} label]
   [:input {:type "text" :id id :onKeyPress action}]
   [:br]])


(defn input-value-changed [v]
  (fn [ev] (reset! v (-> ev (.-target) (.-value)))))

(defn current-pos-display
  "current-pos-display displays the current position given a scan float"
  [scan]
  (fn []
    [:div
     (let [y @scan
           y (if y y 0.0)]
       [:h3 (.toFixed y 2)])]))

(defn text-input [id text action]
  (let [v (atom "")]
    (fn []
      [:div
       [:input {:type "text"
                :id id
                :value @v
                :onChange (input-value-changed v)
                :onKeyPress (form-key-press action)}]
       [:button {:href "#" :onClick #(action v)} text]])))

(defn handle-number-submit [handler]
  (fn [n-str]
    (if-let [n (parse-number @n-str)]
      (handler n))))

(defn number-input [input-name action-name action]
  [text-input input-name action-name
   (handle-number-submit action)])

(defn scan-control-panel
  "scan-control-panel is a component which manages the current scan state"
  [data]
  (let [scan (reagent/cursor data [:voronoi :scan])
        paused (reagent/cursor data [:scan :paused])]
    (fn []
      [:div {:id "control"}
       [current-pos-display scan]
       [button (if @paused "play" "pause") #(toggle-pause-scan! data)]
       [button "next" #(do-step! data)]
       [button "clear" #(reset-state! data [])]
       [number-input "y-val" "set" #(do-set-to! data %)]
       [number-input "by-val" "step" #(do-set-by! data %)]
       [:br]])))


(defn do-ok [getter pred action]
  #(let [v (getter)] (if (pred v) (action v))))

(defn get-form-numbers [ids]
  (let [ids (for [id ids]
              (let [n (get-form-num id)]
                {(keyword id) (get-form-num id)}))
        ]
    (apply merge ids)))

(defn control-add-point [control-funcs]
  (let [])
  (let [point-prop-keys ["point-x" "point-y"]
        point-prop-ids (map keyword point-prop-keys)
        get-point #(get-form-numbers point-prop-keys)
        point-vals (fn [p] (map #(get p %) point-prop-ids))
        point-point (fn [{x :point-x y :point-y}] {:x x :y y})
        point-ok? #(not-any? isNaN? (point-vals %))
        do-reset-point #((:reset-state control-funcs) [(point-point %)])
        reset-point (do-ok get-point point-ok? do-reset-point)
        do-add-point #((:add-points control-funcs) [(point-point %)])
        add-point (do-ok get-point point-ok? do-add-point)]
    [:div
     [:form {:id "point" :onSubmit on-submit-action}
      [:div {:id "point-fields"}
       (make-text-input "point-x" "X" default-form-keypress)
       (make-text-input "point-y" "Y" default-form-keypress)
       [:button {:href "#" :onClick reset-point} "reset"]
       [:button {:href "#" :onClick add-point} "add"]]]]))

(defn control-add-circle [control-funcs]
  (let [circle-prop-keys ["circle-n" "circle-rad" "circle-x" "circle-y"]
        circle-prop-ids (map keyword circle-prop-keys)
        get-circle #(get-form-numbers circle-prop-keys)
        circle-vals (fn [c] (map #(get c %) circle-prop-ids))
        circle-ok? #(not-any? isNaN? (circle-vals %))
        circle-points #(apply points/circle-points (circle-vals %))
        do-reset-circle #((:reset-state control-funcs) (circle-points %))
        reset-circle (do-ok get-circle circle-ok? do-reset-circle)
        do-add-circle #((:add-points control-funcs) (circle-points %))
        add-circle (do-ok get-circle circle-ok? do-add-circle)]
    [:div
     [:form {:id "circle" :onSubmit on-submit-action}
      [:div {:id "circle-fields"}
       (make-text-input "circle-rad" "Radius" default-form-keypress)
       (make-text-input "circle-n" "N" default-form-keypress)
       (make-text-input "circle-x" "X" default-form-keypress)
       (make-text-input "circle-y" "Y" default-form-keypress)
       [:button {:href "#" :onClick reset-circle} "reset"]
       [:button {:href "#" :onClick add-circle} "add"]]
      ]]))


(defn control-add-random [data]
  (let [v (atom "100")
        id "random"
        do-f (fn [f] #(if-let [n (parse-number @v)] (f n)))
        reset-random (do-f #(reset-state! data (points/random-points %)))
        add-random (do-f #(add-points! data (points/random-points %)))]
    (fn []
      [:div
       [:form {:id id :onSubmit on-submit-action}
        [:div
         [:label {:for id} "N"]
         [:input {:type "text"
                  :id id
                  :onKeyPress default-form-keypress
                  :onChange (input-value-changed v)}]]
        [:button {:href "#" :onClick reset-random} "reset"]
        [:button {:href "#" :onClick add-random} "add"]
       ]])))

(defn control-funcs [data]
  {:toggle-pause-scan #(toggle-pause-scan! data)
   :reset-state #(reset-state! data %)
   :clear #(reset-state! data [])
   :step #(do-step! data)
   :set-to  #(let [to (get-form-num "y-val")]
               (if-not (isNaN? to)
                 (do-set-to! data to)))
   :set-by #(let [by (get-form-num "by-val")]
              (if-not (isNaN? by)
                (do-set-by! data by)))
   :add-points #(add-points! data %)})


(defn point-control-panel [data]
  (let [control-funcs (control-funcs data)
        add-mode (atom :random)
        valid-add-values #{"random" "circle" "point"}
        set-add-mode (fn [ev]
                       (if (valid-add-values ev.target.value)
                         (let [kw (keyword ev.target.value)]
                           (reset! add-mode kw))))]
    (fn []
      [:div.point-control-panel
       [:div [:select {:id "add-type"
                       :onChange set-add-mode
                       :defaultValue (str @add-mode)}
              [:option {:value "random"} "Random"]
              [:option {:value "circle"} "Circle"]
              [:option {:value "point"} "Point"]]
        (case @add-mode
          :random [control-add-random data]
          :circle (control-add-circle control-funcs)
          :point (control-add-point control-funcs))]])))


(defn control-panel [data]
  (swap! data #(assoc % :scan {:paused true}))
  (let []
    [:div.control-panel
     [scan-control-panel data]
     [point-control-panel data]]))
