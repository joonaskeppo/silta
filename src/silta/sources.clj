(ns silta.sources
  (:require [clojure.core.async :as a]))

(defonce
  ^{:doc "Registry of mappings of sources to sinks.
    Used to facilitate the generation of new versions of source-based views."}
  source-registry
  (atom {}))

;; NOTE: refactor this later (combine with source-registry)
(defonce renderer-registry
  (atom {}))

;; later: {:source->sinks {...} :sink->renderer {...}}

(defonce
  ^{:doc "Channel for communicating all source changes to SSE stream"}
  source-chan
  (a/chan))

(defprotocol Sourceable
  (connect! [this callback]
    "Ensure `callback` fn is called when source (`this`) updates. Should be idempotent.")
  (disconnect! [this]
    "Sever source's (`this`) connection to `callback`. Should be idempotent.")
  (get-value [this]
    "Return current source (`this`) value, similarly to `deref`."))

(defn source? [x]
  (satisfies? Sourceable x))

(defn add-sink
  [source sink-id renderer]
  (swap! source-registry update source (comp set conj) sink-id)
  (swap! renderer-registry assoc sink-id renderer))

(defn remove-sink
  [source sink-id]
  (swap! source-registry update source disj sink-id))

(defn trigger-update!
  "Trigger SSE update events for all sinks matching `source`"
  [source]
  (let [sink-ids (get @source-registry source)]
    (doseq [sink-id sink-ids
            :let [renderer (get @renderer-registry sink-id)]]
      (a/put! source-chan (renderer)))))

(defn ->value
  "Ensure `p` is coerced into a static value, if Sourceable.
  Returns `p` if not Sourceable."
  [p]
  (if (source? p) (get-value p) p))

(defn setup-sink!
  "Setup sink with provided params.
  Calls sink's renderer when any Sourceable param updates.
  Idempotent; not affected by additional calls due to re-renders."
  [[sink & params :as view+params]]
  (let [sink-id (hash view+params)]
    (when-not (get @renderer-registry sink-id)
      (doseq [source (filter source? params)]
        (add-sink source sink-id (:renderer sink))
        (connect! source trigger-update!)))
    sink-id))

