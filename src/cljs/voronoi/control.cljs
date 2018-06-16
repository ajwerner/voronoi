(ns voronoi.control
  (:require [voronoi.voronoi :as vor]))

(defn do-step! [state-atom]
  (swap! state-atom #(update % :voronoi vor/handle-event)))

(defn pause-scan [scan-state]
  (-> scan-state
      (assoc :paused true)
      (dissoc :last-update)))

(defn pause [state]
  (update-in state [:scan] pause-scan))


(defn reset-scan [state]
  (let [ss (assoc state :scan {:paused true})
        ss (assoc ss :voronoi (vor/new-voronoi (:points ss)))]
    ss))

(defn reset-state [points]
  (let [state (reset-scan
               {:points points
                :display-state {:show-table false}})
        state (pause state)]
    state))

(defn clear-points [state]
  (reset-state []))

(defn set-to-f [yf]
  (fn [state]
    (let [s (:scan (:voronoi state))
          to (yf state)
          state (cond
                  (nil? to) state
                  (>= to s) (update state :voronoi #(vor/scan-to % to))
                  (< to s) (update state :voronoi #(vor/reset-to % to)))]
      state)))

(defn do-set-by-f [by]
  (set-to-f (fn [{{y :scan} :voronoi}] (+ y by))))

(defn do-set-to-f [to]
  (set-to-f #(identity to)))

(defn do-set-by! [state-atom by]
  (swap! state-atom (comp (do-set-by-f by) pause)))

(defn do-set-to! [state-atom to]
  (swap! state-atom (comp (do-set-to-f to) pause)))

(defn now [] (.now js/Date))

(def *interval* 16)
(def *rate* 50)

(defn run-timer-func [start register]
  (fn [{{last :last-update
         paused :paused} :scan
        :as state}]
    (if-not (= last start)
      state
      (let [t (now)
            delta-t (- t last)
            delta (* (/ *rate* 1000.0 ) delta-t)
            do-set-by (do-set-by-f delta)
            state (-> state
                (do-set-by)
                (assoc-in [:scan :last-update] t))
            after (now)
            took (- after t)
            wait (max 0 ( - *interval* took))]
        (register (run-timer-func t register) *interval*)
        state)
      )))

(defn update-pause-scan-state [state update-paused-f state-atom]
  (let [now-paused (update-paused-f (:paused (:scan state)))]
    (if now-paused
      (pause state)
      (let [t (now)
            update-fn (fn [])
            register
            (fn [f interval]
              (js/setTimeout #(swap! state-atom f) interval))
            f (run-timer-func t register)
            s (update-in state [:scan]
                       #(assoc % :paused false :last-update t))
            after (f s)]
        after))))

(defn toggle-pause-scan! [state]
  (swap! state update-pause-scan-state not state))

(defn add-points! [state-atom new-points]
  (swap! state-atom
         #(reset-state (concat (:points (:voronoi %)) new-points))))

(defn reset-state! [state-atom points-lists]
  (reset! state-atom (reset-state points-lists))
  state-atom)

(defn mouse-pressed!
  "Adds a point"
  [state event]
  (swap! state
         (fn [state]
           (if (= (:button event) :right)
             (reset-scan
              (update-in state [:points] #(conj % (select-keys event [:x :y]))))
             state)))
  state)


(defn do-step-by! [state-atom by]
  (swap! state-atom #(pause (update % :voronoi vor/handle-event)))
  state-atom)
