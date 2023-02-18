(ns silta.dsl
  "Functions and tools for handling the event mini-DSL"
  (:require [clojure.string :as str]
            [silta.hiccup :as sh]
            [silta.utils :refer [jsonify]]))

(defn event-attr?
  "Does attribute keyword start with ':on*'?"
  [x]
  (if (string? x)
    (str/starts-with? x "on")
    (str/starts-with? (subs (str x) 1) "on")))

;; TODO: should handle [:myelt#myid ...] formats too
(defn process-attrs
  "Process hiccup attributes.

  Normalizes Reagent-style event attributes ('on-*') into Hiccup-style (`on*`).
  In the end, the 'internal format' doesn't matter, since we prune
  those attributes from the elements before rendering."
  [{:keys [id] :or {id (str "silta-elt-" (random-uuid))} :as attrs}]
  (loop [[k :as ks] (keys attrs)
         attrs attrs
         all-events nil]
    (if (empty? ks)
      (-> (assoc attrs :id id)
          (merge (when (seq all-events)
                   {:silta-events (jsonify all-events)})))
      (let [k* (name k)
            attr-value (get attrs k)]
        ;; don't convert stringified JS in tag
        (if (and (event-attr? k*) (not (string? attr-value)))
          (let [event-type (str/lower-case (subs k* 3))
                serializable-events (->> attr-value
                                         (mapv (fn [event]
                                                 (let [all-but-handler (vec (butlast event))
                                                       [handler & params] (last event)]
                                                   (conj all-but-handler (into [(:endpoint handler)] params))))))]
            (recur (rest ks) (dissoc attrs k) (assoc all-events event-type serializable-events)))
          (recur (rest ks) attrs all-events))))))

(defn transform-hiccup
  [h]
  (sh/edit-hiccup h
                  (fn [elt]
                    (if (sh/hiccup? elt)
                      (if-let [attrs (sh/get-attrs elt)]
                        (sh/set-attrs elt (process-attrs attrs))
                        elt)
                      elt))))

