(ns ^:figwheel-no-load vor2.dev
  (:require
    [vor2.core :as core]
    [devtools.core :as devtools]))

(devtools/install!)

(enable-console-print!)

(core/init!)
