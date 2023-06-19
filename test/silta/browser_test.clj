(ns silta.browser-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.set :as set]
            [garden.selectors :as s]
            [reitit.ring :as ring]
            [aleph.http :as http]
            [wally.main :as w]
            [wally.selectors :as ws]
            [mount.core :as mount]
            [silta.core :refer [make-routes]]
            ;; -- apps --
            [silta.test-apps.basic :as basic-app]
            [silta.test-apps.todo :as todo-app]))

;; --- app ---

(def default-port 3080)

(def all-routes
  (make-routes [["/basic" basic-app/page]
                ["/todo" todo-app/page]]))

(def main-handler
  (ring/ring-handler
   (ring/router all-routes)))

(mount/defstate server
  :start (http/start-server main-handler {:port default-port})
  :stop (when server (.close server)))

(comment
  (mount/start [#'server])
  (mount/stop))

;; --- fixures ---

(defn headless-mode [f]
  (w/with-page (w/make-page {:headless true})
    (f)))

(defn wrap-app [f]
  (mount/start [#'server])
  (f)
  (mount/stop [#'server]))

(use-fixtures :once headless-mode wrap-app)

;; --- tests ---

(deftest test-basic-app
  (w/navigate "http://localhost:3080/basic")

  ;; prior to clicks
  (w/in-viewport? (s/div (ws/text "No clicks here")))
  (w/click (s/button (ws/text "Click me")))

  ;; after one click
  (w/in-viewport? (s/div (ws/text "Clicked 1 times...")))
  (w/click (s/button (ws/text "Thanks, click again?")))

  ;; after two clicks
  (w/in-viewport? (s/div (ws/text "Clicked 2 times...")))
  
  (is true))
