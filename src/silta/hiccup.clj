(ns silta.hiccup
  (:require [clojure.zip :as zip]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [silta.sources]
            [silta.impl.sources]
            [silta.utils :refer [clj->json]]))

(defrecord
 ^{:doc "An atomic view component with optional (potentially side-effectful) `before` and `after` fns.
    See `defview` for further documentation."}
 View
 [context endpoint props renderer])

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
  [[elt :as h] attrs]
  (if attrs
    (if-let [children (get-children h)]
      (into [elt attrs] children)
      [elt attrs])
    (if-let [children (get-children h)]
      (into [elt] children)
      [elt])))

(defn set-children
  "Set a Hiccup vector's child elements as the provided `children`"
  [[elt :as h] children]
  (if-let [attrs (get-attrs h)]
    (into [elt attrs] children)
    (into [elt] children)))

(defn update-attrs
  "Applies `f` to hiccup `h` attributes.
  Optionally applies `args` to fn call."
  ([h f]
   (set-attrs h (f (get-attrs h))))
  ([h f & args]
   (set-attrs h (apply f (into (vector (get-attrs h)) args)))))

(defn update-children
  "Applies `f` to hiccup `h` children."
  [[elt :as h] f]
  (if-let [attrs (get-attrs h)]
    (into [elt attrs] (map f (drop 2 h)))
    (into [elt] (map f (drop 1 h)))))

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

(defn as-event-type
  "Convert attribute into event type.
  Returns event type on success, nil otherwise."
  [x]
  (when (keyword? x)
    (let [x* (name x)]
      (when (str/starts-with? x* "on")
        (let [evt (subs x* 2)]
          (if (str/starts-with? evt "-")
            (str/lower-case (subs evt 1))
            (str/lower-case evt)))))))

(defn get-hiccup-type
  "Infer keyword descriptor of hiccup root node"
  [[elt :as h]]
  (cond
    (sink? elt) :sink
    (view? elt) :view
    :else
    (let [children (get-children h)]
      (if (empty? children)
        :terminal
        (loop [[child :as children] children]
          (cond
            (empty? children) :default
            (sink? child)     :parent-of-sink
            (view? child)     :parent-of-view
            :else             (recur (rest children))))))))
