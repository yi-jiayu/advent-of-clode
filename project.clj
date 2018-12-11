(defproject aoc "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/clojure"]
  :java-source-paths ["src/java" "test/java"]
  :test-paths ["test/clojure"]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/math.combinatorics "0.1.4"]]
  :profiles {:dev    {:plugins      [[lein-cloverage "1.0.13"]
                                     [lein-cljfmt "0.6.1"]]
                      :cloverage    {:test-ns-regex [#"^((?!test-skip-coverage).)*$"]}
                      :dependencies [[org.junit.platform/junit-platform-console-standalone "1.3.2"]
                                     [org.hamcrest/hamcrest-all "1.3"]]}}
  :test-selectors {:default (complement :slow)})
