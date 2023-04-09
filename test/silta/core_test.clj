(ns silta.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [clojure.walk]
            [silta.test-utils :refer [mangle-attrs]]
            [silta.core :refer [defview make-routes]]
            [silta.adapter :refer [*compile-hiccup*]]
            [silta.hiccup :refer [sink? view?]]))

;; --- helpers ---

(defview ^:no-html va
  []
  [:div "Something here"])

(defview ^:no-html vb
  [x y]
  [:div
   [:span (format "x is %s" x)]
   [:span (format "y is %s" y)]])

(defview ^:no-html vb*
  {:before identity}
  [{[x y] :params :as _request}]
  [:div
   [:span (format "x is %s" x)]
   [:span (format "y is %s" y)]])

(defview ^:no-html ^:sink vc
  [x]
  [:div
   [va]
   [vb x x]])

(defonce test-atom
  (atom 1))

(def test-page
  [:div
   [:h1 "Test page"]
   [vc test-atom]])

(defn- non-sink-view?
  [v]
  (and (view? v) (not (sink? v))))

(defn- get-endpoints [pages]
  (->> pages make-routes (map first) set))

(defn- render
  [view & params]
  (binding [*compile-hiccup* false]
    (->> (when (seq params) {:params (vec params)})
         ((:renderer view))
         mangle-attrs)))

;; --- tests ---

;; TODO: test for view body containing several forms, but only last should be hiccup

;; TODO: tests with `on-click`, `on-blur`...

;; TODO: test to check that invoking two views with same params should have both equivalent `silta-view-name` and `silta-view-id`
;;       (same for `silta-sink-id`)

(deftest test-defview
  (testing "with no parameters"
    (is (non-sink-view? va))
    (is (= "/va" (:endpoint va)))
    (is (= [:div {:silta-view-name "silta.core-test/va"
                  :silta-view-id "<id>"}
            "Something here"]
           (render va))))
  (testing "with parameters, no nested views"
    (is (non-sink-view? vb))
    (is (= "/vb" (:endpoint vb)))
    (is (= [:div {:silta-view-name "silta.core-test/vb"
                  :silta-view-id "<id>"}
            [:span "x is 1"]
            [:span "y is 2"]]
           (render vb 1 2))))
  (testing "with full request map"
    (is (non-sink-view? vb*))
    (is (= "/vb*" (:endpoint vb*)))
    (is (= [:div {:silta-view-name "silta.core-test/vb*"
                  :silta-view-id "<id>"}
            [:span "x is 1"]
            [:span "y is 2"]]
           (render vb* 1 2))))
    ;; simply tests def'ing the sink, not actually any live updates
  (testing "with `:sink`"
    (is (sink? vc))
    (is (= "/vc" (:endpoint vc)))
    (is (= [:div {:silta-view-name "silta.core-test/vc"
                  :silta-sink-id "<id>"}
            [:div {:silta-view-name "silta.core-test/va"
                   :silta-view-id "<id>"}
             "Something here"]
            [:div {:silta-view-name "silta.core-test/vb"
                   :silta-view-id "<id>"}
             [:span "x is 1"]
             [:span "y is 1"]]]
           (render vc 1)))))

;; TODO: more comprehensive route tests
(deftest test-make-routes
  (testing "with test page, should set up routes for all views + SSE route due to sink"
    (is (set/subset? #{"/" "/va" "/vb" "/vc"} (get-endpoints [["/" test-page]])))))
