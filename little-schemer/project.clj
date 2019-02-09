(defproject little-schemer "0.1.0-SNAPSHOT"
  :description "Exercises from The Little Schemer"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]
                                  [expound "0.7.2"]]}})
