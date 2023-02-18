(ns user
  (:require [silta.core :refer [defview make-routes]]
            [silta.dsl]
            [silta.hiccup :as sh]
            [reitit.core :as r]
            [reitit.ring :as ring]
            [aleph.http :as http]
            [manifold.stream]
            [hiccup.core :refer [html]]
            [clojure.core.async :as a]
            [clojure.java.io :as io]))

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

(def page
  (let [initial-counter 0]
    [:div
     [notice {:counter initial-counter}]
     [button {:counter initial-counter}]
     [:script (slurp (io/resource "js/base.js"))]]))

;; --- routes ---

(def routes
  (make-routes [["/" page]]))

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

