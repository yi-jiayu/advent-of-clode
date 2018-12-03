(defproject aoc "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/core.async "0.4.474"]]
  :profiles {:dev {:plugins   [[lein-cloverage "1.0.13"] [lein-cljfmt "0.6.1"]]
                   :cloverage {:test-ns-regex [#"^((?!test-skip-coverage).)*$"]}}}
  :test-selectors {:default (complement :slow)})
