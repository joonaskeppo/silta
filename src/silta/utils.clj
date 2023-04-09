(ns silta.utils
  "Common utility fns"
  (:require [clojure.string :as str]
            [jsonista.core :as j]))

(defn update-if-key
  [m k f]
  (if (contains? m k)
    (update m k f)
    m))

(defn map-vals
  [f m]
  (->> m
       (map (fn [[k v]]
              [k (f v)]))
       (into {})))

(defn map-keys
  [f m]
  (->> m
       (map (fn [[k v]]
              [(f k) v]))
       (into {})))

(defn json->clj
  "Convert (seq of) JSON into Clojure data using jsonista"
  [v]
  (cond
    (string? v)     (try
                      (j/read-value v j/keyword-keys-object-mapper)
                      (catch Exception _ v))
    (sequential? v) (mapv json->clj v)
    :else           v))

(def clj->json
  "Convert clj data into a JSON string using jsonista"
  j/write-value-as-string)

(defmacro try-chain
  "Executes `forms` sequentially, wrapped in try-catch blocks.
  Returns first valid result."
  [forms]
  `(loop [fs# ~(mapv (fn [form] `(fn [] ~form)) forms)]
     (when (seq fs#)
       (let [result# (try
                       ((first fs#))
                       (catch Exception e# :silta.utils/exception))]
         (if (not= :silta.utils/exception result#)
           result#
           (recur (rest fs#)))))))

(comment
  (try-chain [(require '[hiccup.core])
              (require '[not-real.core])])
  )
