(ns silta.core
  (:require [silta.hiccup]
            [silta.sse]
            [silta.utils :refer [map-vals]]
            [silta.adapter]
            [silta.html]
            [reitit.ring.middleware.parameters :refer [parameters-middleware]]
            [jsonista.core :as j]
            [clojure.string :as str]
            [clojure.java.io :as io]))

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
  "Create the renderer fn form, dependent on metadata.
  Tries to pre-process as much of `body` as possible."
  [{:keys [body]
    {:keys [view-name] :as context} :context
    {:keys [before after]} :props}]
  (let [[_ unqualified-name] (str/split view-name #"/")
        renderer-name (symbol (str "render-" unqualified-name))
        process-opts (merge context {:view-name view-name
                                     :view-sym (symbol unqualified-name)})
        params-sym (symbol (gensym "params"))
        main-fn `(fn ~renderer-name [~params-sym]
                   (let [f# (fn ~(symbol (str "inner-" renderer-name))
                              ~(:arglist context)
                              (silta.adapter/process
                                ~(assoc process-opts :params params-sym)
                                ~body))]
                     ;; For convenience (see: renderer arglist), apply if vectorized :params
                     ;; (this may be a bad idea...)
                     (if (vector? ~params-sym)
                       (apply f# ~params-sym)
                       (f# ~params-sym))))
        update-params (fn [{:keys [params] :or {params []} :as req}]
                        (assoc req :params (or (some-> params :__params json->clj)
                                               params)))]
    `(comp ~after ~main-fn ~before ~update-params)))

(defn- get-view-arg
  [args pred predated-by]
  (when-let [?arg (-> (count-not-nils predated-by) (drop args) first)]
    (when (pred ?arg) ?arg)))

(def default-props
  ;; pull out `:params` from (HTTP) request by default
  {:before :params
   :after identity})

(defmacro defview
  "Create a view component
  Invoked similarly to `defn`.

  Metadata may be provided that alters how the view is used.
  Accepted boolean metadata:
  * `:sink`, transforms the view into a sink type when true

  Optional params:
  * `docstring`, as first argument
  * `condition-map`, after `arglist`; recognized keys are `:before`, `:after`, and `:path`"
  [vname & args]
  (let [docstring (get-view-arg args string? [])
        props (get-view-arg args map? [docstring])
        arglist (get-view-arg args vector? [docstring props])
        body (get-view-arg args any? [docstring props arglist])
        metadata (update (meta vname) :doc #(or % docstring))
        endpoint (make-endpoint (assoc props :name vname))
        final-props (merge default-props props)
        qualified-name (format "%s/%s" *ns* vname)
        renderer (make-renderer {:context {:arglist arglist
                                           :no-html (boolean (:no-html metadata))
                                           :sink (boolean (:sink metadata))
                                           :view-name qualified-name}
                                 :props final-props
                                 :body body})]
    (assert (vector? arglist) (format "Missing arglist from view '%s'" vname))
    `(do
       (def ~(with-meta vname metadata)
         (silta.hiccup.View.
          ~(merge (select-keys metadata [:sink])
                  {:name qualified-name})
          ~endpoint
          ~final-props
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
                   :handler (comp respond-html handler)}}])

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

(defn get-default-page-opts []
  "Default page options.
  Used for configuring routes and client-side JavaScript."
  {:append-client-js true
   :sse (get-default-sse-setting)})

;; TODO: we should also handle page-specific opts to disable/enable SSE, etc.
;; (however, `make-routes` should work more or less as-is; more to do with JS side...)
(defn make-routes
  [pages]
  (let [default-page-opts (get-default-page-opts)
        live-routes [["/stream" silta.sse/sse-handler]]
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
