(ns silta.utils
  "Common utility fns"
  (:require [clojure.string :as str]))

(defn update-if-key
  [m k f]
  (if (contains? m k)
    (update m k f)
    m))

(defn map-vals
  [m f]
  (->> m
       (map (fn [[k v]]
              [k (f v)]))
       (into {})))

(defn map-keys
  [m f]
  (->> m
       (map (fn [[k v]]
              [(f k) v]))
       (into {})))

(defn jsonify
  "Quick and dirty JSON serialization for a small subset of Clojure.
  Used for transforming Silta data into JSON for frontend consumption,
  via Silta HTML tags."
  [x]
  (cond
    (map? x)          (->> x
                           (map (fn [[k v]]
                                  (format "%s: %s" (jsonify k) (jsonify v))))
                           (str/join ", ")
                           (format "{%s}"))
    (sequential? x)   (->> x (map jsonify) (str/join ", ") (format "[%s]"))
    (keyword? x)      (->> (subs (str x) 1) (format "\"%s\""))
    (number? x)       (str x)
    (string? x)       (format "\"%s\"" x)
    :else             (throw (ex-info "Unable to serialize data!"
                                      {:data x :type (type x)})))) 


