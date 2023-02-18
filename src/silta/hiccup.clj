(ns silta.hiccup
  (:require [clojure.zip :as zip]
            [clojure.walk :as walk]))

(defrecord
 ^{:doc "An atomic view component with optional (potentially side-effectful) `before` and `after` fns.
    See `defview` for further documentation."}
 View
 [endpoint before after renderer])

(defn view? [x]
  (instance? View x))

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

(defn expand-hiccup
  "Expand views inside hiccup"
  ([h]
   (expand-hiccup nil h))
  ([req h]
   (edit-hiccup h
    (fn [[elt :as h]]
      (cond
        (view? elt) (let [{:keys [renderer]} elt
                          args (rest h)
                          req* (if (and (= 1 (count args)) (not (coll? (first args))))
                                 (assoc req :params (first args))
                                 (update req :params into args))]
                      (expand-hiccup req (renderer req*)))
        :else       h)))))
