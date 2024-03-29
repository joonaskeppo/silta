(ns user
  (:require [silta.core :refer [defview make-routes]]
            [silta.hiccup :as sh]
            [silta.sources :as ss]
            [reitit.core :as r]
            [reitit.ring :as ring]
            [aleph.http :as http]
            [manifold.stream]
            [clojure.core.async :as a]
            [clojure.tools.namespace.repl :as tn]
            [mount.core :as mount]))

(defview intro-text []
  [:div
   [:span "Hello there! "]
   [:span "This is a a test."]])

;; sinks are similar to ordinary views,
;; except that they must be provided valid, derefable *sources* as inputs
;; (simple, self-contained SSE mechanism, no knowledge of client state)

(defview ^:sink test-sink
  [xv] ;; `xv` is a derefed value
  [:div
   [:span xv]])

;; by default, only show `:params` from req, for convenience.
;; if we want the full request map, we should redefine `:before`

(defview notice
  "This is a notice"
  {:before (fn [req]
             (->  req
                  (assoc :client-id (get-in req [:headers "client-id"] " (not received yet)"))))}
  [{[{:keys [counter]}] :params :keys [client-id] :as req}]
  [:div {:id "notice"}
   [:div (format "Client id: %s" client-id)]
   [:div
    (if (zero? counter)
      "No clicks here"
      (format "Clicked %s times..." counter))]])

(defview test-appender
  {:before identity}
  [{[{:keys [counter]}] :params}]
  [:span (apply str (take counter (repeat ".")))])

(defview button
  {:after (fn [res]
            (tap> [:button/after res])
            res)}
  [counter]
  [:button {:data-test "click-me"
            :on-click [[:swap {:target "#notice"}
                        [notice {:counter (inc counter)}]]  ;; replaces arbitrary elements with querySelectorAll
                       ;; FIXME: need to have a way to deal with sequential updates
                       #_[:append {:target "#notice"}
                          [test-appender {:counter (inc counter)}]] ;; append dots
                       [:swap
                        [button (inc counter)]]]} ;; replace this specific `button` DOM element
   (if (zero? counter)
     "Click me"
     "Thanks, click again?")])

(defonce +example-source+
  (atom "hello"))

(def page
  (let [initial-counter 0]
    [:div
     [intro-text]
     [test-sink +example-source+]
     [notice {:counter initial-counter}]
     [button initial-counter]]))

(comment
  ((:renderer button) {:params [0]}))

;; --- routes ---

(def routes
  ;; we can trigger sse on/off
  (make-routes [["/" #_{:sse false} page]]))

;; --- app ---

(def default-port 3030)

(def main-handler
  (ring/ring-handler
   (ring/router routes)))

(mount/defstate server
  :start (http/start-server main-handler {:port default-port})
  :stop (when server (.close server)))

(defn go []
  (mount/start)
  :ready)

(defn reset []
  (mount/stop)
  (tn/refresh :after 'user/go)
  :reset)

(defn stop []
  (mount/stop)
  :stopped)

(comment
  (go)
  (reset))

(comment
  ;; update value, should be reflected in sink view
  (reset! +example-source+ "NEW VALUE"))
