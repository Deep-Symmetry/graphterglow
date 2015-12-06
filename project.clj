(defproject graphterglow "0.1.0-SNAPSHOT"
  :description "A simple project for drawing graphs of Afterglow behavior."
  :url "https://github.com/brunchboy/graphterglow"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [afterglow "0.1.6-SNAPSHOT"]
                 [incanter "1.9.0"]]
  :profiles {:dev {:repl-options {:init-ns graphterglow.core
                                  :welcome (println "Graph test environment loaded.")}
                   :jvm-opts ["-XX:-OmitStackTraceInFastThrow"]}})
