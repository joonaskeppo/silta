(ns silta.adapter
  "Macro-based facilities to process and adapt hiccup for Silta consumption.
  Implementation heavily inspired by Hiccup's compiler."
  (:require [silta.hiccup :as sh]
            [silta.html :refer [html]]
            [silta.sources :as sources]
            [silta.utils :refer [clj->json map-vals]]
            [clojure.walk :as walk]))

;; TODO: split into two namespaces -> adapter.comptime, adapter.runtime
;; so process becomes `silta.adapter.comptime/adapt` (a macro)
;; and adapt becomes `silta.adapter.runtime/adapt` (a fn)

;; TODO: also, make sure that hiccup compiler not called prematurely...
;; (add tests)

(def ^:dynamic *compile-hiccup* true)

(declare process-any adapt)

;; --- forms ---

(defn- get-form-name [form]
  ;; doesn't do anything for keywords used as fns, e.g., `(:thing x)`
  (when (and (seq? form) (symbol? (first form)))
    (name (first form))))

(defmulti ^:private process-form
  "Process by form type"
  get-form-name)

(defmethod process-form "let"
  [[_ bindings & body]]
  `(let ~bindings
     ~@(butlast body)
     ~(process-any (last body))))

;; no-op (already wrapped in `adapt`)
(defmethod process-form "adapt"
  [form]
  form)

;; by default, defer to runtime 
(defmethod process-form :default
  [form]
  `(adapt ~form))

;; --- hiccup ---

(defn- unevaluated?
  "True if the expression has not been evaluated"
  [expr]
  (or (symbol? expr)
      (and (seq? expr)
           (not= (first expr) `quote))))

(defn- literal?
  "True if x is a literal that can be evaluated as-is"
  [x]
  (and (not (unevaluated? x))
       (or (not (or (vector? x) (map? x)))
           (record? x) ;; e.g., a view
           (every? literal? x))))

(defn literal-attrs?
  "Does `h` have a literal attributes map?
  A map of literals, `nil` or a non-existent map returns true."
  [[_ ?attrs :as _h]]
  (or (literal? ?attrs) (vector? ?attrs)))

(defn- try-eval-params
  [params]
  (mapv (fn [x]
          (if (symbol? x)
            (try
              (eval x)
              (catch Exception _ x))
            x))
        params))

(def ^:private ^:const dynamic-identifier
  "__silta-dynamic")

(defn- prepare-view-args
  "Transform any dynamic args into a format recognized by client.
  Dynamic args are values in `args` that are in hiccup format."
  [args]
  (map-vals (fn [x]
              (if (sh/hiccup? x)
                (let [f-name (subs (str (first x)) 1)] ; :name/space -> "name/space"
                  (into [dynamic-identifier f-name] (rest x)))
                x))
            args))

(comment
  (prepare-view-args {:some-input-value [:value "#my-div"]
                      :as-is ["some" "values"]}))

(defn adapt-events
  "Adapt event handlers for attribute map `attrs`"
  [attrs]
  (let [event-keys-and-types (->> attrs
                                  keys
                                  (keep (fn [k]
                                          (when (not (string? (get attrs k)))
                                            (when-let [event-type (sh/as-event-type k)]
                                              [k event-type])))))
        events (->> (for [[k event-type] event-keys-and-types
                          :let [events (get attrs k)]]
                      [event-type (for [event events
                                        :let [last-idx (dec (count event))
                                              ?h (get event last-idx)
                                              [elt :as h] (when (vector? ?h) ?h)]]
                                    (if (and h (silta.hiccup/view? elt))
                                      (let [serialized-view (-> h (update 0 :endpoint) (sh/update-attrs prepare-view-args))]
                                        (assoc event last-idx serialized-view))
                                      ;; add event 
                                      event))])
                    (into {}))]
    (cond-> (apply dissoc attrs (map first event-keys-and-types))
      (seq events) (assoc :data-silta-events (clj->json events)))))

(defn adapt-attrs
  [attrs]
  (adapt-events attrs))

(comment
  (adapt-events {:on-click [[:prepend {:target "#todo-list:first-child"}
                             [:div {:text [:value "#new-todo"]
                                    :checked false}]]]})
  (adapt-events {:on-click [[:dom.node/remove {:target "#todo-1"}]]}))

(defn extract-values
  "Transform sources found within `x` into their values"
  [x]
  (walk/postwalk sources/->value x))

(defn coerce-params
  "Ensure params are formatted correctly"
  [params]
  (let [params (extract-values params)]
    (cond
      (sequential? params) (vec params)
      (map? params) (vector params)
      :else (throw (ex-info "Invalid param formatting! Must be a vector or map." params)))))

(defn adapt-view-invocation
  [{:keys [renderer] :as _elt} params]
  (adapt (renderer {:params (coerce-params params)})))

;; this will only handle the invocation from a parent view
;; attaching attributes and setting things up is handled elsewhere
(defn- process-view-invocation
  [view-elt [view-sym & params :as _unresolved-view]]
  (if (every? literal? params)
    (let [params (try-eval-params params)
          elt-val (if (var? view-elt) (var-get view-elt) view-elt)]
      (process-any
        ((:renderer elt-val) {:params (coerce-params params)})))
    `(adapt ((:renderer ~view-sym)
             {:params (coerce-params ~(vec params))}))))

;; NOTE:
;; simplifying assumption: every view/sink needs a top-level element
;; -> can't be list of hiccup
;; this is for ensuring consistency across renders without any surprise node injections
;; that might bork `:target` selectors, etc.

(defn- process-hiccup
  [h]
  (if (literal-attrs? h)
    (-> h (sh/update-children process-any) (sh/update-attrs adapt-attrs))
    (let [h (sh/update-children h process-any)]
      `(let [h# ~h
             new-attrs# (adapt-attrs (sh/get-attrs h#))]
         (sh/set-attrs h# new-attrs#)))))

(defn process-root-view-node
  "Process the root node of a view (incl. sink).
  Adds attributes common to every invocation of view."
  [{:keys [view-name view-sym view-id sink]} form]
  (let [form* (process-any form)
        ;; FIXME: can improve on `const-attrs` (also: move into separate fn -> test)
        ;; (e.g., now returning false when second elt is hiccup w/ inner forms)
        const-attrs (and (vector? form*) (every? literal? (take 2 form*)))
        new-attrs {:data-silta-view-name view-name
                   :data-silta-view-type (if sink "sink" "view")
                   :data-silta-view-id view-id}]
    (if const-attrs
      (let [old-attrs (sh/get-attrs form*)]
        (->> (merge old-attrs new-attrs)
             adapt-attrs
             (sh/set-attrs form*)
             process-any))
      `(let [h# ~form ; must be original form to get attrs
             old-attrs# (sh/get-attrs h#)
             new-attrs# ~new-attrs]
         (->> (merge old-attrs# new-attrs#)
              adapt-attrs
              (sh/set-attrs h#)
              adapt)))))

;; --- top-level handlers ---

(defn- with-adapted-attrs
  "Returns hiccup with adapted attributes"
  [h]
  (->> h
       silta.hiccup/get-attrs
       adapt-attrs
       (silta.hiccup/set-attrs h)))

(defn- with-expanded-views
  "Returns hiccup with expanded view"
  [[elt :as h]]
  (if (silta.hiccup/view? elt)
    (let [args (rest h)
          args (if (and (= 1 (count args)) (map? (first args)))
                 (first args)
                 (vec args))]
      (adapt-view-invocation elt args))
    h))

(def with-adapted-hiccup
  (comp with-expanded-views with-adapted-attrs))

(defn- adapt*
  [x]
  (let [is-hiccup (silta.hiccup/hiccup+? x)
        x (cond-> x
            is-hiccup silta.hiccup/unroll-hiccup)]
    (cond
      is-hiccup
      (silta.hiccup/edit-hiccup x with-adapted-hiccup)

      (and (sequential? x) (every? silta.hiccup/hiccup+? x))
      (map adapt x)

      (sequential? x)
      (into (vec (butlast x)) [(adapt (last x))])

      :else
      x)))

(defn compile-hiccup? [form]
  (and *compile-hiccup* (not (string? form))))

(defn adapt
  "Adapt hiccup into Silta-compatible form.
  Returns compiled HTML string if `*compile-hiccup*` is true."
  [x]
  (let [adapted (adapt* x)]
    adapted ;; TODO: check
    #_(if (compile-hiccup? adapted)
      (html adapted)
      adapted)))

(defn- process-any
  "Process arbitrary forms and hiccup into compatible shape"
  [x]
  (letfn [(maybe-hiccup? [x]
            (and (vector? x)
                 (let [[?elt] x]
                   (or (keyword? ?elt)
                       (symbol? ?elt)
                       (var? ?elt)))))
          (eval-hiccup [h]
            (try
              (update h 0 (fn [elt]
                            (if (literal? elt)
                              elt
                              (resolve elt))))
              (catch Exception _ nil)))]
    (cond
      (maybe-hiccup? x)
      (if-let [[elt :as h] (eval-hiccup x)]
        (let [elt-val (if (var? elt) (var-get elt) elt)]
          (if (sh/view? elt-val)
            (process-view-invocation elt x)
            (process-hiccup h)))
        `(adapt ~x)) ;; NOTE: could be something like [(if this view-1 view-2) param-1 param-2]

      (and (seq? x) (maybe-hiccup? (first x)))
      (map process-any x)

      (seq? x)
      (process-form x)

      (symbol? x)
      (try
        (process-any (eval x))
        (catch Exception _ x))

      :else
      x)))

(defmacro process
  "Transform view render fn body.
  When wrapped inside fn, returns HTML string at runtime, if `*compile-hiccup*` is true.
  If `*compile-hiccup*` is false, returns (form transformable into) Hiccup.
  Defers uninferred or leftover forms to runtime."
  [& args]
  (assert (#{1 2} (count args)) "Must provide 1 or 2 args to `process`")
  (let [opts (when (map? (first args)) (first args))
        form (if opts (second args) (first args))]
    (binding [*compile-hiccup* (not (:no-html opts))]
      (let [processed-hiccup (if (:view-sym opts)
                               (process-root-view-node opts form)
                               (process-any form))]
        (if (compile-hiccup? processed-hiccup)
          `(html ~processed-hiccup)
          processed-hiccup)))))

