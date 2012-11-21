(defproject goldberg "0.1.0-SNAPSHOT"
  :description "The Goldberg Variations in Overtone."
  :dependencies	[
    [org.clojure/clojure "1.4.0"]
    [leipzig "0.1.0" :exclusions [seesaw]]
    [overtone "0.7.1" :exclusions [seesaw]]
  ]
)
