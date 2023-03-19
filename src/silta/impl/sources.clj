(ns silta.impl.sources
  "Built-in source implementations"
  (:require [silta.sources :refer [Sourceable]]))

(extend-protocol Sourceable
  clojure.lang.Atom
  (connect! [this callback]
    (let [key (hash callback)]
      (add-watch this key (fn [_ the-atom _ _] (callback the-atom)))
      (alter-meta! this update ::callbacks (comp set conj) key)))
  (disconnect! [this]
    (doseq [key (::callbacks (meta this))]
      (remove-watch this key))
    (alter-meta! this dissoc ::callbacks))
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
