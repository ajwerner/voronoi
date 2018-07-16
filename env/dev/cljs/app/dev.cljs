(ns ^:figwheel-no-load voronoi.dev
  (:require
    [app.core :as core]
    [devtools.core :as devtools]))

(devtools/install!)

(enable-console-print!)

(core/init!)
