(defproject heen-en-terug "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.374"]]
  :main ^:skip-aot heen-en-terug.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
