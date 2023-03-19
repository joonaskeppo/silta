(ns silta.test-utils
  (:require [clojure.walk]))

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
