(ns silta.test-utils
  (:require [clojure.walk]
            [silta.adapter :refer [process]]))

(defn mangle-attrs
  [h]
  (clojure.walk/postwalk
   (fn [x]
     (if (map? x)
       (cond-> x
         (contains? x :silta-view-id) (assoc :silta-view-id "<id>")
         (contains? x :silta-sink-id) (assoc :silta-sink-id "<id>"))
       x))
   h))

(defmacro -process
  "Wrapper over `process` for testing purposes"
  [& args]
  (let [default-opts {:no-html true}
        use-opts (map? (first args))
        opts (if use-opts
               (merge default-opts (first args))
               default-opts)
        body (if use-opts (rest args) args)]
  `(mangle-attrs
     (process ~opts ~@body))))
