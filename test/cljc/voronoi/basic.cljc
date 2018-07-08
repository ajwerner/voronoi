(ns voronoi.basic
  (:require #?(:clj [clojure.test :refer [deftest run-tests run-all-tests is]]
               :cljs [cljs.test :refer-macros [deftest is testing run-tests run-all-tests]])
            [voronoi.voronoi :as vor]
            [voronoi.points :as points]))

(deftest test-it
  (let [size 10000]
    (time
      (vor/finish-builder (vor/new-voronoi-builder (points/random-points size))))))
