(ns silta.browser-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.set :as set]
            [clojure.edn :as edn]
            [garden.selectors :as s]
            [reitit.ring :as ring]
            [aleph.http :as http]
            [wally.main :as w]
            [wally.selectors :as ws]
            [mount.core :as mount]
            [silta.core :refer [make-routes]]
            ;; -- apps --
            [silta.test-apps.counter :as counter-app]
            [silta.test-apps.todo :as todo-app]
            [silta.test-apps.form :as form-app]))

;; --- app ---

(def default-port 3080)

(def all-routes
  (make-routes [["/counter" counter-app/page]
                ["/todo" todo-app/page]
                ["/form" form-app/page]]))

(def main-handler
  (ring/ring-handler
   (ring/router all-routes)))

(mount/defstate server
  :start (http/start-server main-handler {:port default-port})
  :stop (when server (.close server)))

(comment
  ;; reset
  (do
    (mount/stop)
    (mount/start [#'server]))

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

;; --- utils ---

(defn- text-content [selector]
  (.textContent (w/-query selector)))

(defn- inner-text [selector]
  (.innerText (w/-query selector)))

(defn- input-value [selector]
  (.inputValue (w/-query (s/input selector))))

;; --- tests ---

(defn app-url
  [app]
  (format "http://localhost:%s/%s" default-port app))

(deftest test-counter-app
  (w/navigate (app-url "counter"))

  ;; prior to clicks
  (w/in-viewport? (s/div (ws/text "No clicks here")))
  (w/click (s/button (ws/text "Click me")))

  ;; after one click
  (w/in-viewport? (s/div (ws/text "Clicked 1 times...")))
  (w/click (s/button (ws/text "Thanks, click again?")))

  ;; after two clicks
  (w/in-viewport? (s/div (ws/text "Clicked 2 times...")))
  
  (is true))

(deftest test-todo-app
  (w/navigate (app-url "todo"))
  
  ;; prior to actions
  (w/in-viewport? (s/s (ws/text "Make todo app")))
  (w/in-viewport? (s/span (ws/text "Make it pretty")))

  ;; toggle first item status
  (w/click (s/> (s/li (s/first-of-type))
                (s/button (ws/text "Still in progress"))))
  (w/in-viewport? (s/span (ws/text "Make todo app")))

  ;; remove both items
  (is (seq (text-content (s/ul "#todo-list"))))
  (w/click (s/> (s/li (s/nth-of-type "2"))
                (s/button (ws/text "Remove"))))
  (w/click (s/> (s/li (s/first-of-type))
                (s/button (ws/text "Remove"))))
  (is (empty? (text-content (s/ul "#todo-list"))))

  ;; add new items
  (letfn [(add-todo [item]
            (w/fill (s/input (s/attr= "data-test" "add-todo-input")) item)
            (is (= item (input-value (s/attr= "data-test" "add-todo-input"))))
            (w/keyboard-press "Enter")
            (is (empty? (input-value (s/attr= "data-test" "add-todo-input"))))
            (w/in-viewport? (s/span (ws/text item))))]
    (add-todo "get milk")
    (add-todo "get cookies"))
  
  ;; toggle first item
  (w/click (s/> (s/li (s/first-of-type))
                (s/button (ws/text "Completed"))))
  (w/in-viewport? (s/> (s/li (s/first-of-type))
                       (s/button (ws/text "Still in progress")))))

(deftest test-form-app
  (w/navigate (app-url "form"))

  (is (empty? (text-content (w/-query (s/div (s/attr= "id" "form-summary"))))))
  
  (w/fill (s/input (s/attr= "id" "some-input")) "Some text")
  (w/click (s/input (s/attr= "id" "a-checkbox")))
  (w/click (s/input (s/attr= "type" "submit")))

  (is (= {:a-checkbox true :some-input "Some text"}
         (edn/read-string (inner-text (s/p (s/attr= "id" "data-string")))))))
