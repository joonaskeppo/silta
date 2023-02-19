(ns silta.impl.sources
  "Built-in source implementations"
  (:require [silta.sources :refer [Sourceable]]))

(extend-protocol Sourceable
  clojure.lang.Atom
  (connect! [this callback]
    (add-watch this ::source-change (fn [_ the-atom _ _] (callback the-atom))))
  (disconnect! [this]
    (remove-watch this ::source-change))
  (get-value [this]
    (deref this)))

(comment
  (require '[silta.sources :refer [connect! disconnect! trigger-update! source?]])

  (def +example-source+
    (atom 0))

  (connect! +example-source+ trigger-update!)

  (disconnect! +example-source+)

  ;; trigger update event
  (swap! +example-source+ inc))
