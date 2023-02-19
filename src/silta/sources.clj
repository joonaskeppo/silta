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
  (connect! [this callback] "Call callback fn when source updates")
  (disconnect! [this] "Sever connection to callback")
  (get-value [this] "Return current source value, like `deref`"))

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

(defn setup-sink!
  "Setup sink with provided params, and `renderer` to call when one of the provided sources (one or more `params`) updates"
  [[_sink & params :as sink+params] renderer]
  (assert (every? source? params)) ;; for now..? NOTE: unnecessary restriction, will remove later
  (let [sink-id (hash sink+params)]
    (doseq [source params]
      (add-sink source sink-id renderer)
      (connect! source trigger-update!))
    sink-id))

