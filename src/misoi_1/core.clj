(ns misoi-1.core
  (:gen-class))

(use 'clusters.clusterize)
(use 'alex-and-georges.debug-repl)
(require '[clojure.string :as str])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (out_clusterize))
)
