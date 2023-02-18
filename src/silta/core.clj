(ns silta.core
  (:require [hiccup.core :as h]
            [silta.hiccup]
            [silta.dsl]
            [reitit.ring.middleware.parameters :refer [parameters-middleware]]))

(defonce
  ^{:doc "Registry of all view components.
    Used to generate HTML-returning routes with their respective handlers."}
  view-registry
  (atom {}))

(defn- make-endpoint
  "Generate endpoint route for view"
  [{:keys [path name] :as props}]
  (or path (str "/" name)))

(def ^:private count-not-nils
  (comp count (partial filter some?)))

(defn- make-renderer
  "Create the renderer fn form, dependent on metadata"
  [vname metadata arglist body]
  (let [renderer-name (symbol (str "render-" vname))
        main-fn `(fn ~renderer-name ~arglist ~@body)]
    (cond
      (:with-req metadata) main-fn
      :else                `(fn [req#] (~main-fn (:params req#))))))

;; TODO: should try to be half-smart about compiling as much as possible
;; of `body` -- what can't be inferred should be handled at runtime.
(defmacro defview
  "Instruments a view component.
  Invoked similarly to `defn`.

  Optional params:
  * `docstring`, as first argument
  * `condition-map`, after `arglist`; recognized keys are `:before`, `:after`, and `:path`"
  [vname & args]
  (let [docstring (when (string? (first args)) (first args))
        arglist (if docstring
                  (when (vector? (second args)) (second args))
                  (when (vector? (first args)) (first args)))
        props (when-let [?props (first (drop (count-not-nils [docstring arglist]) args))]
                (when (map? ?props) ?props))
        body (drop (count-not-nils [docstring arglist props]) args)
        metadata (update (meta vname) :doc #(or % docstring))
        endpoint (make-endpoint (assoc props :name vname))
        renderer (make-renderer vname metadata arglist body)]
    (assert (every? seq [arglist body]))
    `(do
       (def ~(with-meta vname metadata)
         (silta.hiccup.View. ~endpoint ~(:before props) ~(:after props) ~renderer))
       (swap! view-registry assoc ~endpoint ~vname)
       ~vname)))

(defn- render
  [page]
  (->> page
       silta.hiccup/expand-hiccup
       silta.dsl/transform-hiccup
       h/html))

(defn- respond-html
  [html]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body html})

;; TODO: expand/improve on this (currently only used for case in user ns)
(defn- coerce
  [x]
  (try
    ;; TODO: float
    (let [x* (Integer/parseInt x)]
      x*)
  (catch Exception _
    x)))

(defn- coerce-params-middleware
  [handler & _args]
  (fn [req]
    (-> req
        (update :params (comp (partial into {})
                              (partial map (fn [[k v]]
                                             [(keyword k) (coerce v)]))))
        handler)))

(defn- make-route
  "Make Reitit route definition"
  [[endpoint handler]]
  [endpoint {:get {:middleware [parameters-middleware
                                coerce-params-middleware]
                   :handler (comp respond-html render handler)}}])

(defn make-routes
  [pages]
  ;; TODO: mapv-vals
  (let [views (mapv (fn [[k v]]
                      [k (:renderer v)])
                    @view-registry)
        pages (mapv (fn [[k v]]
                      [k (if (fn? v) v (constantly v))])
                    pages)]
    (mapv make-route (into pages views))))

(comment
  (make-routes [["/" [:div "sup"]]]))

;; TODO:
;; - a let-like form (e.g., `let-views`) for small, one-off views (that still require routing, obv)
;; - `view` and `sink` macros (may be redundant to have `let-views`..?)
