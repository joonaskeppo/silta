(ns silta.core
  (:require [hiccup.core :as h]
            [silta.hiccup]
            [reitit.ring.middleware.parameters :refer [parameters-middleware]]
            [jsonista.core :as j]))

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
        main-fn `(fn ~renderer-name ~arglist ~@body)
        get-params (fn [req]
                     (some-> req :params :__params (j/read-value j/keyword-keys-object-mapper)))
        update-params (fn [req]
                        (if-let [params (get-params req)]
                          (assoc req :params params)
                          req))]
    (cond
      (:with-req metadata) `(comp ~main-fn ~update-params)
      :else                `(fn [req#]
                              (let [params# (or (~get-params req#) (:params req#))]
                                (apply ~main-fn params#))))))

;; TODO: should try to be half-smart about compiling as much as possible
;; of `body` -- what can't be inferred should be handled at runtime.
(defmacro defview
  "Create a view component
  Invoked similarly to `defn`.

  Metadata may be provided that alters how the view is used.
  Accepted boolean metadata:
  * `:with-req`, provides unaltered request map to view as input
  * `:sink`, transforms the view into a sink type

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
        renderer (make-renderer vname metadata arglist body)
        context (select-keys metadata [:sink])]
    (assert (every? seq [arglist body]))
    `(do
       (def ~(with-meta vname metadata)
         (silta.hiccup.View. ~context ~endpoint ~(:before props) ~(:after props) ~renderer))
       (swap! view-registry assoc ~endpoint ~vname)
       ~vname)))

(defn- render
  ([page]
   (render page nil))
  ([page req] ;; TODO: is `req` actually used currently?
   (-> page (silta.hiccup/prepare-hiccup req render) h/html)))

(defn- respond-html
  [html]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body html})

;; TODO: just use jsonista
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

;; TODO:
;; - a let-like form (e.g., `let-views`) for small, one-off views (that still require routing, obv)
;; - `view` and `sink` macros (may be redundant to have `let-views`..?)
