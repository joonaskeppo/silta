(ns silta.core
  (:require [silta.hiccup]
            [silta.sse]
            [silta.utils :refer [map-vals]]
            [hiccup.core :as h]
            [reitit.ring.middleware.parameters :refer [parameters-middleware]]
            [jsonista.core :as j]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [clojure.set :as set]))

(defonce
  ^{:doc "Registry of all view components.
    Used to generate HTML-returning routes with their respective handlers."}
  view-registry
  (atom {}))

(defn- make-endpoint
  "Generate endpoint route for view"
  [{:keys [path name] :as _props}]
  (or path (str "/" name)))

(def ^:private count-not-nils
  (comp count (partial filter some?)))

(defn- json->clj
  "Convert (seq of) JSON into Clojure data"
  [v]
  (cond
    (string? v)     (try
                      (j/read-value v j/keyword-keys-object-mapper)
                      (catch Exception _ v))
    (sequential? v) (mapv json->clj v)
    :else           v))

(defn- make-renderer
  "Create the renderer fn form, dependent on metadata"
  [vname metadata arglist body]
  (let [renderer-name (symbol (str "render-" vname))
        main-fn `(fn ~renderer-name ~arglist ~@body)
        get-params (fn [req]
                     (some-> req :params :__params json->clj))
        update-params (fn [req]
                        (if-let [params (get-params req)]
                          (assoc req :params params)
                          req))]
    (cond
      (:with-req metadata) `(comp ~main-fn ~update-params)
      :else                `(fn [req#]
                              (let [params# (or (~get-params req#) (:params req#))]
                                (apply ~main-fn params#))))))

;; these inferences are unused as of now
;; they don't generalize fully (symbol may not be resolve properly to a view)
(comment
  (defn- infer-views*
    "Naive inference of views contained within hiccup.
  Does not recur, infers views only in current view."
    [h]
    (let [views-found (atom #{})
          add-view    #(do (swap! views-found conj %1) %2)]
      (walk/postwalk
       (fn [x]
         (cond
           (silta.hiccup/view? x) (add-view x x)
           (symbol? x)            (try
                                    (let [?view (-> x resolve var-get)]
                                      (if (silta.hiccup/view? ?view)
                                        (add-view ?view x)
                                        x))
                                    (catch Exception _ x))
           :else                  x))
       h)
      @views-found))

  (defn infer-views
    "Infer views contained within view or hiccup.
  Does not recur, infers views only in current view/hiccup."
    [x]
    (cond
      (silta.hiccup/view? x)    (infer-views* (get-in x [:context :body]))
      (silta.hiccup/hiccup+? x) (infer-views* x)
      :else                          (throw (ex-info "Unable to infer views for data:"
                                                     {:data x}))))

  (defn infer-all-views
    "Infer all views recursively"
    ([x]
     (infer-all-views x #{}))
    ([x views-so-far]
     (let [next-views       (infer-views x)
           unexplored-views (set/difference next-views views-so-far)
           all-known-views  (set/union views-so-far next-views)]
       (if (seq unexplored-views)
         (set (mapcat #(infer-all-views % all-known-views) unexplored-views))
         all-known-views)))))

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
        renderer (make-renderer vname metadata arglist body)]
    (assert (and (vector? arglist) (seq body)))
    `(do
       (def ~(with-meta vname metadata)
         (silta.hiccup.View.
          (merge ~(select-keys metadata [:sink])
                 {:arglist '~arglist :name '~vname})
          ~endpoint
          ~(:before props) ~(:after props)
          ~renderer))
       (swap! view-registry assoc ~endpoint ~vname)
       ~vname)))

(def js-files
  (->> {:base "base.js"
        :sse "sse.js"}
       (map-vals (comp slurp io/resource (partial str "js/")))))

(defn bundle-js
  "Get Silta client-side JS script as string"
  ([]
   (bundle-js {:sse true}))
  ([{:keys [sse] :as _opts}]
   (if sse
     (->> js-files vals (str/join "\n"))
     (->> js-files :base))))

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

(defn- coerce-params-middleware
  [handler & _args]
  (fn [req]
    (-> req
        (update :params (partial reduce-kv (fn [m k v]
                                             (assoc m (keyword k) (json->clj v)))
                                 {}))
        handler)))

(defn- make-route
  "Make Reitit route definition"
  [[endpoint handler]]
  [endpoint {:get {:middleware [parameters-middleware
                                coerce-params-middleware]
                   :handler (comp respond-html render handler)}}])

(defn- append-js
  "Adds script tag to hiccup page definition"
  [opts page]
  (conj page [:script (bundle-js opts)]))

(defn- get-default-sse-setting
  "Should SSE be set up by default?
  Returns true if view registry contains at least one sink."
  []
  (->> @view-registry
       (some (comp silta.hiccup/sink? second))
       boolean))

(defn- use-sse?
  "Should we add SSE for page?"
  [page-opts]
  (if (contains? page-opts :sse)
    (:sse page-opts)
    (get-default-sse-setting)))

(def default-page-opts
  "Default page options.
  Used for configuring routes and client-side JavaScript."
  {:append-client-js true
   :sse true})

;; TODO: we should also handle page-specific opts to disable/enable SSE, etc.
;; (however, `make-routes` should work more or less as-is; more to do with JS side...)
(defn make-routes
  [pages]
  (let [live-routes [["/stream" silta.sse/sse-handler]]
        page-routes (->> pages
                         (mapv (fn [[endpoint & rem]]
                                 (let [page (last rem)
                                       opts (if (map? (first rem))
                                              (merge default-page-opts (first rem))
                                              default-page-opts)]
                                   [endpoint (constantly
                                              (if (:append-client-js opts)
                                                (append-js opts page)
                                                page))]))))
        view-routes (->> @view-registry
                         (mapv (fn [[endpoint view]]
                                 [endpoint (:renderer view)])))]
    (->> (into page-routes view-routes)
         (mapv make-route)
         (into live-routes))))

;; TODO:
;; - a let-like form (e.g., `let-views`) for small, one-off views (that still require routing, obv)
;; - `view` and `sink` macros (may be redundant to have `let-views`..?)
