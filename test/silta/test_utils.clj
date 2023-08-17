(ns silta.test-utils
  (:require [clojure.walk]
            [silta.adapter :refer [process]]))

(defn mangle-attrs
  [h]
  (->> h
       (clojure.walk/postwalk
         (fn [x]
           (if (map? x)
             (cond-> x
               (contains? x :data-silta-view-id) (assoc :data-silta-view-id "<id>"))
             x)))))

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
