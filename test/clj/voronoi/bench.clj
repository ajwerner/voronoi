(ns voronoi.bench
  (:require [clojure.test :refer [deftest run-tests run-all-tests is]]
            [voronoi.core :as vor]
            [voronoi.points :as points]
            [criterium.core :as criterium]))


(defn test-random [size]
  (println size)
  (time
   (criterium/with-progress-reporting
     (criterium/bench
      (vor/finish-builder (vor/new-voronoi-builder (points/random-points size)))
      :verbose)))
  nil)

(deftest test-random-small
  (test-random 10)
  (test-random 20)
  (test-random 40)
  (test-random 80)
  (test-random 160)
  (test-random 320)
  (test-random 640)
  (test-random 1280))

(run-all-tests)
