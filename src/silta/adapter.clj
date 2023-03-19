(ns silta.adapter
  "Macro-based facilities to process and adapt hiccup for Silta consumption.
  Implementation heavily inspired by Hiccup's compiler."
  (:require [silta.hiccup :as sh]
            [silta.sources :as sources]
            [silta.utils :refer [clj->json]]))

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
              (sources/->value (eval x))
              (catch Exception _ x))
            x))
        params))

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
                                      (let [serialized-view (update h 0 :endpoint)]
                                        (assoc event last-idx serialized-view))
                                      ;; add event 
                                      [event-type event]))])
                    (into {}))]
    (cond-> (apply dissoc attrs (map first event-keys-and-types))
      (seq events) (assoc :silta-events (clj->json events)))))

(defn adapt-attrs
  [attrs]
  (adapt-events attrs))

(defn make-view-id
  [elt params]
  (format "%s-%s" (get-in elt [:context :name]) (hash params)))

(defn- adapt-view-invocation
  [elt params]
  (let [elt (if (var? elt) (var-get elt) elt)
        view-attrs (if (sh/sink? elt)
                     {:silta-sink-id (sources/setup-sink! (into [elt] params))}
                     {:silta-view-id (make-view-id elt params)})
        renderer (:renderer elt)
        h (process-any (renderer {:params (mapv sources/->value params)}))]
    (sh/update-attrs h merge view-attrs)))

;; this will only handle the invocation from a parent view
;; attaching attributes and setting things up is handled elsewhere
(defn- process-view-invocation
  [[elt & params :as _view]]
  (let [params (try-eval-params params)]
    (if (every? literal? params)
      (adapt-view-invocation elt params)
      (let [elt-val (if (var? elt) `(var-get ~elt) elt)]
        `(let [view-attrs# ~(if (sh/sink? (if (var? elt) (var-get elt) elt))
                              {:silta-sink-id `(sources/setup-sink! ~(into [elt-val] params))}
                              {:silta-view-id `(make-view-id ~elt-val ~params)})
               h# (adapt ((:renderer ~elt-val)
                          {:params (mapv sources/->value ~params)}))]
           (sh/update-attrs h# merge view-attrs#))))))

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
  [form view-name]
  (let [x (process-any form) ;; might be as hiccup, might be as unevaluated form
        sufficiently-processed (and (vector? x) (every? literal? (take 2 x)))
        new-attrs {:silta-view-name view-name}]
    (if sufficiently-processed
      (let [old-attrs (sh/get-attrs x)]
        (process-any (sh/set-attrs x (adapt-attrs (merge old-attrs new-attrs)))))
      `(let [h# ~x
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
    (adapt (adapt-view-invocation elt (rest h)))
    h))

(def with-adapted-hiccup
  (comp with-expanded-views with-adapted-attrs))

(defn adapt
  "Adapt hiccup into Silta-compatible form.
  Returns original form if unidentified."
  [x]
  (letfn [(resolve-value [x]
            (cond
              (var? x)    (var-get x)
              (symbol? x) (try (var-get (resolve x)) (catch Exception _ x))
              (vector? x) (mapv resolve-value x)
              :else       x))]
    (let [x (resolve-value x)]
      (cond
        (silta.hiccup/hiccup+? x)
        (silta.hiccup/edit-hiccup x with-adapted-hiccup)

        (and (seq? x) (every? silta.hiccup/hiccup+? x))
        (map adapt x)

        ;; only transform last item (could be body with side effects)
        (seq? x)
        (concat (butlast x) [(adapt (last x))])

        :else
        x))))

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
        (let [elt (if (var? elt) (var-get elt) elt)]
          (if (sh/view? elt)
            (process-view-invocation h)
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
  "Transform view render fn body into a usable form.
  Will defer any uninferred or leftover forms to runtime.
  `mode` must be one of `:default`, `:sink`, or `:view`."
  [& args]
  (assert (#{1 2} (count args)) "Must provide 1 or 2 args to `process`")
  (let [{:keys [view-name] :as opts} (when (map? (first args)) (first args))
        form (if opts (second args) (first args))]
    (if view-name
      (process-root-view-node form view-name)
      (process-any form))))

