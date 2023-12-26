(ns silta.hiccup
  (:require [clojure.zip :as zip]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [silta.sources]
            [silta.impl.sources]
            [silta.utils :refer [clj->json]])
  (:import [clojure.lang IFn]))

(defrecord
  ^{:doc "An atomic view component with optional (potentially side-effectful) `before` and `after` fns.
    See `defview` for further documentation."}
  View
  [context endpoint props renderer]

  IFn
  (invoke [_]
    (renderer {:params []}))
  (invoke [_ arg1]
    (renderer {:params [arg1]}))
  (invoke [_ arg1 arg2]
    (renderer {:params [arg1 arg2]}))
  (invoke [_ arg1 arg2 arg3]
    (renderer {:params [arg1 arg2 arg3]}))
  (invoke [_ arg1 arg2 arg3 arg4]
    (renderer {:params [arg1 arg2 arg3 arg4]}))
  (invoke [_ arg1 arg2 arg3 arg4 arg5]
    (renderer {:params [arg1 arg2 arg3 arg4 arg5]}))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6]
    (renderer {:params [arg1 arg2 arg3 arg4 arg5 arg6]}))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7]
    (renderer {:params [arg1 arg2 arg3 arg4 arg5 arg6 arg7]}))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8]
    (renderer {:params [arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8]}))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9]
    (renderer {:params [arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9]}))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10]
    (renderer {:params [arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10]}))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11]
    (renderer {:params [arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11]}))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12]
    (renderer {:params [arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12]}))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13]
    (renderer {:params [arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13]}))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14]
    (renderer {:params [arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14]}))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15]
    (renderer {:params [arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15]}))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16]
    (renderer {:params [arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16]}))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17]
    (renderer {:params [arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17]}))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18]
    (renderer {:params [arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18]}))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19]
    (renderer {:params [arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19]}))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 arg20]
    (renderer {:params [arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 arg20]}))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 arg20 res]
    (apply renderer (into [arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 arg20] res)))
  (applyTo [_ args]
    (apply renderer args)))

(defn view? [x]
  (instance? View x))

(defn sink? [x]
  (boolean
   (and (view? x)
        (get-in x [:context :sink]))))

;; TODO: remove?
(defn hiccup?
  [?h]
  (and (vector? ?h)
       (keyword? (first ?h))))

(defn hiccup+?
  "Regular hiccup, but also recognize views and seqs of hiccup
  (e.g., generated via `for`)"
  [?h]
  (or (and (vector? ?h)
           (let [elt (first ?h)]
             (or (keyword? elt)
                 (view? elt))))
      (and (seq? ?h)
           (hiccup+? (first ?h)))))

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
  (if (seq attrs)
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
  (zip/zipper hiccup+? get-children set-children root))

(defn edit-hiccup
  "Edit all Hiccup elements in `h` with `(f element)`"
  [h f]
  (loop [zipper (hiccup-zip h)]
    (cond
      (zip/end? zipper)    (zip/root zipper)
      (zip/branch? zipper) (recur (zip/next (zip/edit zipper f)))
      :else                (recur (zip/next zipper)))))

(defn unroll-hiccup
  "Unrolls any seqs inside hiccup"
  [h]
  (reduce (fn [acc x]
            (if (seq? x)
              (into acc x)
              (conj acc x)))
          []
          h))

(comment
  (def *h* [:ul {:test 1} "jepu" (for [i (range 3)] [:li i])])
  (edit-hiccup *h* identity))

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
