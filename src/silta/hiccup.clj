(ns silta.hiccup
  (:require [clojure.zip :as zip]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [silta.sources]
            [silta.impl.sources]
            [silta.utils :refer [jsonify]]))

(defrecord
 ^{:doc "An atomic view component with optional (potentially side-effectful) `before` and `after` fns.
    See `defview` for further documentation."}
 View
 [context endpoint before after renderer])

(defn view? [x]
  (instance? View x))

(defn sink? [x]
  (boolean
   (and (view? x)
        (get-in x [:context :sink]))))

(defn hiccup?
  [?h]
  (and (vector? ?h)
       (keyword? (first ?h))))

(defn hiccup+?
  "Regular hiccup, but also recognize views"
  [?h]
  (and (vector? ?h)
       (let [elt (first ?h)]
         (or (keyword? elt)
             (view? elt)))))

(defn get-attrs
  [[_ ?attrs]]
  (when (map? ?attrs) ?attrs))

(defn get-children
  [h]
  (if (get-attrs h)
    (seq (drop 2 h))
    (seq (drop 1 h))))

(defn set-attrs
  "Set a Hiccup vector's attributes as the provided `attrs`"
  [[elt-type :as h] attrs]
  (if-let [children (get-children h)]
    (into [elt-type attrs] children)
    [elt-type attrs]))

(defn set-children
  "Set a Hiccup vector's child elements as the provided `children`"
  [[elt :as h] children]
  (if-let [attrs (get-attrs h)]
    (into [elt attrs] children)
    (into [elt] children)))

(defn hiccup-zip
  "Create a zipper for extended Hiccup traversal"
  [root]
  (zip/zipper hiccup+?
              get-children
              set-children
              root))

(defn edit-hiccup
  "Edit all Hiccup elements in `h` with `(f element)`"
  [h f]
  (loop [zipper (hiccup-zip h)]
    (cond
      (zip/end? zipper)    (zip/root zipper)
      (zip/branch? zipper) (recur (zip/next (zip/edit zipper f)))
      :else                (recur (zip/next zipper)))))

(defn event-attr?
  "Does attribute keyword start with ':on*'?"
  [x]
  (if (string? x)
    (str/starts-with? x "on")
    (str/starts-with? (subs (str x) 1) "on")))

;; TODO:
  ;; - if source and not sink -> `get-value`
  ;; - if sink -> call `setup-sink!` and add sink-id 

(defn- process-event!
  [event]
  (let [last-idx (dec (count event))
        [view & params :as sink+params] (get event last-idx)
        serialized-view (update sink+params 0 :endpoint)]
    {::events [(assoc event last-idx serialized-view)]}))

;; TODO: should handle [:myelt#myid ...] formats too
(defn- process-attrs
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
          (let [event-type        (str/lower-case (subs k* 3))
                attrs             (->> attr-value (map process-event!) (apply merge-with into) (merge attrs))]
            (recur (rest ks) (dissoc attrs k ::events) (assoc all-events event-type (get attrs ::events))))
          (recur (rest ks) attrs all-events))))))

(defn- generate-attrs
  [h]
  (if (hiccup? h)
    (if-let [attrs (get-attrs h)]
      (set-attrs h (process-attrs attrs))
      h)
    h))

(defn- get-value
  "Ensure `p` is coerced into a static value"
  [p]
  (if (silta.sources/source? p)
    (silta.sources/get-value p)
    p))

(defn- render-identity
  [thing req]
  thing)

(defn- expand-hiccup
  "Expand views inside hiccup, setup sinks with `render`"
  ([h]
   (expand-hiccup h nil render-identity))
  ([h req render]
   (edit-hiccup h
                (fn [[elt :as h]]
                  (cond
                    (view? elt) (let [{:keys [renderer]} elt
                                      args (->> (rest h) (map get-value))
                                      req* (update req :params into args)
                                      root (renderer req*)
                                      attrs (merge (get-attrs root)
                                                   ;; TODO: will need to refactor later
                                                   ;; this sets up the sink for subsequent renders
                                                   (when (sink? elt)
                                                     (let [renderer (fn [] (render h req))]
                                                       {:silta-sink-id (silta.sources/setup-sink! h renderer)})))
                                      root (set-attrs root attrs)]
                                  (expand-hiccup root req render))
                    :else       h)))))

(defn prepare-hiccup
  ([h]
   (prepare-hiccup h nil render-identity))
  ([h req render]
   (-> h
       (expand-hiccup req render)
       (edit-hiccup generate-attrs))))
