(defproject fun-data-structures "0.1.0-SNAPSHOT"
  :description "Exercises from Purely Functional Data Structures"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]
                                  [org.clojure/core.match "0.3.0-alpha5"]
                                  [expound "0.7.2"]]}})
