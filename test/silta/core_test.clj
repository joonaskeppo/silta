(ns silta.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [clojure.string :as str]
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

;; FIXME: if we reverse `va` and `vb` calling order,
;; that changes the resultant hiccup (likely since `vb` called with params, and `va` is constant)
(defview ^:no-html ^:sink vc
  [x]
  [:div
   [va]
   [vb x x]])

(def boom (atom :bip))

(defview ^:no-html side-effectful
  []
  (reset! boom :BAP)
  [:div
   [:span "This is still rendered"]])

(comment
  (silta.adapter/adapt
    (silta.adapter/adapt
      ((:renderer vb) {:params [1 1]})))

  ((:renderer side-effectful) {:params []})
  ((:renderer vc) {:params [1]})
  ((:renderer va) {:params []})
  ((:renderer vb*) {:params [1 2]}))

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

(deftest test-defview
  (testing "with no parameters"
    (is (non-sink-view? va))
    (is (str/ends-with? (:endpoint va) "/core-test/va"))
    (is (= [:div {:data-silta-view-name "silta.core-test/va"
                  :data-silta-view-type "view"
                  :data-silta-view-id "<id>"}
            "Something here"]
           (render va))))
  (testing "with parameters, no nested views"
    (is (non-sink-view? vb))
    (is (str/ends-with? (:endpoint vb) "/core-test/vb"))
    (is (= [:div {:data-silta-view-name "silta.core-test/vb"
                  :data-silta-view-type "view"
                  :data-silta-view-id "<id>"}
            [:span "x is 1"]
            [:span "y is 2"]]
           (render vb 1 2))))
  (testing "with full request map"
    (is (non-sink-view? vb*))
    (is (str/ends-with? (:endpoint vb*) "/core-test/vb*"))
    (is (= [:div {:data-silta-view-name "silta.core-test/vb*"
                  :data-silta-view-type "view"
                  :data-silta-view-id "<id>"}
            [:span "x is 1"]
            [:span "y is 2"]]
           (render vb* 1 2))))
  ;; simply tests def'ing the sink, not actually any live updates
  (testing "with `:sink`"
    (is (sink? vc))
    (is (str/ends-with? (:endpoint vc) "/core-test/vc"))
    (is (= [:div {:data-silta-view-name "silta.core-test/vc"
                  :data-silta-view-type "sink"
                  :data-silta-view-id "<id>"}
            [:div {:data-silta-view-name "silta.core-test/va"
                   :data-silta-view-type "view"
                   :data-silta-view-id "<id>"}
             "Something here"]
            [:div {:data-silta-view-name "silta.core-test/vb"
                   :data-silta-view-type "view"
                   :data-silta-view-id "<id>"}
             [:span "x is 1"]
             [:span "y is 1"]]]
           (render vc 1))))
  (testing "with side-effectful view"
    (is (= [:div {:data-silta-view-name "silta.core-test/side-effectful"
                  :data-silta-view-type "view"
                  :data-silta-view-id "<id>"}
            [:span "This is still rendered"]]
           (render side-effectful)))
    (is (= :BAP @boom))))

;; TODO: more comprehensive route tests
(deftest test-make-routes
  (testing "with test page, should set up routes for all views + SSE route due to sink"
    (is (set/subset? #{"/core-test/va"
                       "/core-test/vb"
                       "/core-test/vb*"
                       "/core-test/vc"
                       "/core-test/side-effectful"}
                     (->> (get-endpoints [["/" test-page]])
                          (filter #(re-find #"core-test/" %))
                          (map #(str/replace % #"/silta" ""))
                          set)))))
