(ns silta.sse
  (:require [manifold.stream]
            [clojure.core.async :as a]
            [silta.sources]))
            

(defn sse-handler
  "Ring handler for an SSE stream endpoint"
  [_req]
  ;; NOTE: this version almost verbatim copied from:
  ;; https://gist.github.com/jeroenvandijk/67d064e0bb08b900e656
  ;; this is for testing purposes only; will eventually create a new version
  ;; from scratch...
  (let [ping-result (Object.)
        xform (comp (mapcat (fn [x]
                              (if (= x ping-result)
                                ["ping"]
                                [x])))
                    (map (fn [msg]
                           (str "\ndata:" msg "\n"))))
        formatted-chan (a/chan 1 xform)]
    (a/go-loop
     []
      (a/<! (a/timeout 1000))
      (if (a/>! formatted-chan ping-result)
        (recur)
        (println "Channel closed!")))
    (a/pipe silta.sources/source-chan formatted-chan)
    {:status 200
     :body (manifold.stream/->source formatted-chan)
     :headers {"Content-Type" "text/event-stream;charset=UTF-8"
               "Cache-Control" "no-cache, no-store, max-age=0, must-revalidate"
               "Pragma" "no-cache"}}))
