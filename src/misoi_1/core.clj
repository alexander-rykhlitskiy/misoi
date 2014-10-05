(ns misoi-1.core
  (:gen-class))

(use 'clusters.clusterize)
(use 'alex-and-georges.debug-repl)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (debug-repl)
  (println "Hello, World!")
  (println (out_clusterize)))
