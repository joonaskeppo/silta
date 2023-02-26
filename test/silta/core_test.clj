(ns silta.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [silta.core :refer [defview make-routes infer-all-views]]
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

(defview ^:with-req vb*
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

(def test-page-1
  [:div
   [:h1 "Test page"]
   [vc test-atom]])

(def test-page-2
  [va])

(defn- non-sink-view?
  [v]
  (and (view? v) (not (sink? v))))

(defn- get-endpoints [pages]
  (->> pages make-routes (map first) set))

(def endpoints-1
  (get-endpoints [["/" test-page-1]]))

(def endpoints-2
  (get-endpoints [["/" test-page-2]]))

(comment
  (map (comp :name :context) (infer-all-views vc))
  (map (comp :name :context) (infer-all-views test-page-1)))

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
  (testing "with `:with-req`"
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

;; FIXME: for whatever reason, if we invoke the `get-endpoints` fn within deftest, we get an incomplete result for the first assertion
(deftest test-make-routes
  (is (= #{"/" "/va" "/vb" "/vc"} endpoints-1))
  (is (= #{"/" "/va"} endpoints-2)))
