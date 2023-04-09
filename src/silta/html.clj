(ns silta.html
  (:require [silta.utils :refer [try-chain]]))

(def ^:private html-compilers
  "Recognized hiccup-to-HTML compilers.
  Mapping of namespace to macro or fn."
  {'hiccup.core 'html})

(defn- get-compiler []
  (eval
   `(try-chain
     ~(mapv (fn [[ns compiler]]
              `(do
                 (require ~`'[~ns])
                 '~(symbol (str ns) (str compiler))))
            html-compilers))))

(defn- make-compilation-form [body]
  (when-let [compiler (get-compiler)]
    `(~compiler ~body)))

(defmacro html
  [body]
  (or (make-compilation-form body)
      (throw (Exception. "No recognized hiccup-to-HTML compiler found in deps (currently, only Hiccup proper is recognized)."))))

(comment
  (get-compiler)
  (macroexpand '(html [:div "span"])))
