(ns user
  (:require [silta.core :refer [defview make-routes]]
            [silta.hiccup :as sh]
            [silta.sources :as ss]
            [reitit.core :as r]
            [reitit.ring :as ring]
            [aleph.http :as http]
            [manifold.stream]
            [hiccup.core :refer [html]]
            [clojure.core.async :as a]
            [clojure.java.io :as io]))

;; sinks are similar to ordinary views,
;; except that they must be provided valid, derefable *sources* as inputs
;; (simple, self-contained SSE mechanism, no knowledge of client state)

(defview ^:sink test-sink
  [xv] ;; `xv` is a derefed value
  [:div
   [:span xv]])

;; by default, only show `:params` from req, for convenience. if we want the full request map, we should 
;; decorate it with the `^:with-req` meta prop

(defview ^:with-req notice
  [{{:keys [counter]} :params :as p}]
  [:div {:id "notice"}
   (if (zero? counter)
     "No clicks here"
     (format "Clicked %s times..." counter))])

(defview ^:with-req button
  [{{:keys [counter]} :params :as p}]
  [:button {:on-click [[:swap {:target "#notice"}
                        [notice {:counter (inc counter)}]] ;; replaces arbitrary elements with querySelectorAll
                       [:swap
                        [button {:counter (inc counter)}]]]} ;; replace this specific `button` DOM element
   (if (zero? counter)
     "Click me"
     "Thanks, click again?")])

(defonce +example-source+ (atom "hello"))

(def page
  (let [initial-counter 0]
    [:div
     [test-sink +example-source+]
     [notice {:counter initial-counter}]
     [button {:counter initial-counter}]
     [:script (slurp (io/resource "js/base.js"))]]))

(comment
  (require '[silta.core :refer [render]])
  (sh/prepare-hiccup page)

  ;; update value, should be reflected in sink view
  (reset! +example-source+ "UPDATED VALUE"))

;; --- routes ---

(def routes
  (into (make-routes [["/" page]])
        [["/stream" (fn [req]
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
                        (a/pipe ss/source-chan formatted-chan)
                        {:status 200
                         :body (manifold.stream/->source formatted-chan)
                         :headers {"Content-Type" "text/event-stream;charset=UTF-8"
                                   "Cache-Control" "no-cache, no-store, max-age=0, must-revalidate"
                                   "Pragma" "no-cache"}}))]]))

;; --- app ---

(def default-port 3030)

(def main-handler
  (ring/ring-handler
   (ring/router routes)))

(defonce server (atom nil))

(defn start-app
  ([]
   (start-app default-port))
  ([port]
   (let [app (http/start-server main-handler {:port port})]
     (reset! server app))))

(defn stop-app []
  (when @server
    (.close @server)
    (reset! server nil)))

(comment
  (start-app)
  (stop-app))

