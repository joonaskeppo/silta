(ns silta.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [silta.core :refer [defview make-routes]]
            [silta.hiccup :refer [sink? view?]]))

;; --- helpers ---

(defview va
  []
  [:div "Something here"])

(defview vb
  [x y]
  [:div
   [:span (format "x is %s" x)]
   [:span (format "y is %s" y)]])

(defview vb*
  {:before identity}
  [{[x y] :params :as _request}]
  [:div
   [:span (format "x is %s" x)]
   [:span (format "y is %s" y)]])

(defview ^:sink vc
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

;; --- tests ---

(deftest test-defview
  (testing "with no parameters"
    (is (non-sink-view? va))
    (is (= "/va" (:endpoint va)))
    (is (= [:div "Something here"]
           ((:renderer va) nil))))
  (testing "with parameters, no nested views"
    (is (non-sink-view? vb))
    (is (= "/vb" (:endpoint vb)))
    (is (= [:div
            [:span "x is 1"]
            [:span "y is 2"]]
           ((:renderer vb) {:params [1 2]}))))
  (testing "with full request map"
    (is (non-sink-view? vb*))
    (is (= "/vb*" (:endpoint vb*)))
    (is (= [:div
            [:span "x is 1"]
            [:span "y is 2"]]
           ((:renderer vb*) {:params [1 2]}))))
  ;; simply tests def'ing the sink, not actually any live updates
  (testing "with `:sink`"
    (is (sink? vc))
    (is (= "/vc" (:endpoint vc)))
    (is (= [:div
            [va]
            [vb 1 1]]
           ((:renderer vc) {:params [1]})))))

(deftest test-make-routes
  (testing "with test page, should set up routes for all views + SSE route due to sink"
    (is (set/subset? #{"/" "/va" "/vb" "/vc"} (get-endpoints [["/" test-page]])))))
