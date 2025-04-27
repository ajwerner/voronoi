(defproject voronoi "0.1.0"
  :description "A Voronoi Diagram Reagent Project"
  :url "http://ajwerner.github.io/app"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/clojurescript "1.11.132"
                  :scope "provided"]
                 [com.cognitect/transit-cljs "0.8.280"]
                 [com.cognitect/transit-java "1.0.371"]
                 [re-com "2.24.2"]
                 [org.clojure/data.avl "0.2.0"]
                 [testdouble/clojurescript.csv "0.8.0"]
                 [reagent "1.3.0"]
                 [reagent-utils "0.3.8"]
                 [ring "1.14.1"]
                 [ring/ring-core "1.14.1"]
                 [ring-server "0.5.0"]
                 [secretary "1.2.3"]
                 [criterium "0.4.6"]
                 [venantius/accountant "0.2.5"
                  :exclusions [org.clojure/tools.reader]]
                 [compojure "1.7.1"]
                 [hiccup "2.0.0-RC5"]
                 [org.clojure/core.async "1.8.741"]
                 [yogthos/config "1.2.1"]
                 [cljsjs/react "18.3.1-1"]
                 [cljsjs/react-dom "18.3.1-1"]
                 [cljsjs/topojson "1.6.18-0"]
                 [cljsjs/d3geo "0.2.15-2"]
                 [cljsjs/d3 "7.6.1-0"]
                 [cljsjs/csv "1.1.1-0"]
                 [day8.re-frame/http-fx "0.2.4"]
                 [cljs-ajax "0.8.4"]
                 [ring/ring-defaults "0.6.0"]
                 [cljs-http "0.1.48"]
                 [re-frame "1.4.3"]]

  :plugins [[lein-environ "1.1.0"]
            [lein-cljsbuild "1.1.8"]
            [lein-ancient "1.0.0-RC4-SNAPSHOT"]
            [lein-asset-minifier "0.4.7"
             :exclusions [org.clojure/clojure]]]

  :ring {:handler app.handler/app
         :uberwar-name "app.war"}

  :min-lein-version "2.5.0"
  :uberjar-name "app.jar"
  :main app.server
  :clean-targets ^{:protect false}
  [:target-path
   [:cljsbuild :builds :app :compiler :output-dir]
   [:cljsbuild :builds :app :compiler :output-to]]

  :source-paths ["src/clj" "src/cljc"]
  :test-paths ["test/clj" "test/cljc"]
  :resource-paths ["resources" "target/cljsbuild"]

  :minify-assets
  {:assets
   {"resources/public/css/site.min.css" "resources/public/css/site.css"}}

  :cljsbuild
  {:builds {:test
            {:source-paths ["src/cljs" "src/cljc" "test/cljc" "test/cljs"]
             :compiler {:output-to "resources/public/js/testable.js"
                        :main voronoi.runner
                        :optimizations :advanced
                        :npm-deps {:topojson-client "3.0.2"}
                        :install-deps true}}
            :min
            {:source-paths ["src/cljs" "src/cljc" "env/prod/cljs"]
             :compiler
             {:main             "app.core"
              :output-to        "target/cljsbuild/public/js/app.js"
              :output-dir       "target/cljsbuild/public/js"
              :source-map       "target/cljsbuild/public/js/app.js.map"
              :optimizations :advanced
              :pretty-print  false}}
            :app
            {:source-paths ["src/cljs" "src/cljc" "env/dev/cljs"]
             :figwheel {:on-jsload "app.core/mount-root"}
             :compiler
             {:main "app.dev"
              :asset-path "/js/out"
              :output-to "target/cljsbuild/public/js/app.js"
              :output-dir "target/cljsbuild/public/js/out"
              :source-map true
              :optimizations :none
              :pretty-print  true
              :npm-deps {:topojson "3.0.2"}
              :install-deps true
              :preloads [day8.re-frame-10x.preload]
              :closure-defines {"re_frame.trace.trace_enabled_QMARK_" true}}}}}

  :aliases {"fig" ["trampoline" "run" "-m" "figwheel.main"]}

  :figwheel
  {:http-server-root "public"
   :server-port 3449
   :nrepl-port 7002
   :nrepl-middleware ["cider.piggieback/wrap-cljs-repl"]
   :css-dirs ["resources/public/css"]
   :ring-handler app.handler/app
   :repl-eval-timeout 100000}
  :profiles {:dev {:repl-options {:init-ns voronoi.repl
                                  :nrepl-middleware [cider.piggieback/wrap-cljs-repl]}

                   :dependencies [[binaryage/devtools "1.0.7"]
                                  [ring/ring-mock "0.4.0"]
                                  [ring/ring-devel "1.14.1"]
                                  [prone "2021-04-23"]
                                  [org.clojure/clojurescript "1.11.132"]
                                  [org.clojure/core.async "1.8.741"]
                                  [com.bhauman/figwheel-main "0.2.20"]
                                  [org.clojure/tools.nrepl "0.2.13"]
                                  [com.bhauman/rebel-readline-cljs "0.1.5"]
                                  [pjstadig/humane-test-output "0.11.0"]
                                  [day8.re-frame/re-frame-10x "1.10.0"]]

                   :source-paths ["env/dev/clj"]
                   :plugins [[lein-figwheel "0.5.20"]
                             [lein-cljsbuild "1.1.8"]
                             [lein-doo "0.1.10"]]
                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]

                   :env {:dev true}}

             :uberjar {:hooks [minify-assets.plugin/hooks]
                       :source-paths ["env/prod/clj"]
                       :prep-tasks ["compile" ["cljsbuild" "once" "min"]]
                       :env {:production true}
                       :aot :all
                       :omit-source true}})
