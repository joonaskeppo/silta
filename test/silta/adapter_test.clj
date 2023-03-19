(ns silta.adapter-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [silta.test-utils :refer [mangle-attrs]]
            [silta.hiccup :as sh]
            [silta.core :refer [defview]]
            [silta.utils :refer [clj->json]]
            [silta.adapter :refer [process adapt adapt-attrs]]))

;; --- helpers ---

(defview va
  [x]
  [:div (format "got: %s" x)])

(defview vb
  [x y]
  [:div [:span x y]])

(defview ^:sink vsink
  [x]
  [:span x])

;; --- tests ---

(deftest test-adapt-attrs
  (testing "with nothing"
    (is (empty? (adapt-attrs nil)))
    (is (empty? (adapt-attrs {}))))
  (testing "with ordinary attrs"
    (let [attrs {:class "some-class" :id "my-id"}]
      (is (= attrs (adapt-attrs attrs)))))
  (testing "with `on-click`"
    (is (= {:class "my-class unchanged"
            :silta-events (clj->json {:click [[:swap {:target "some-target"} ["/va" "new"]]]})}
           (adapt-attrs {:class "my-class unchanged"
                         :on-click [[:swap {:target "some-target"} [va "new"]]]})))))

(deftest test-adapt
  (testing "with nothing"
    (is (= nil (adapt nil))))
  (testing "with ordinary hiccup"
    (let [h [:span "hello"]]
      (is (= h (adapt h))))
    (let [h [:div
             [:span "hello"]
             [:div {:class "my-class"}
              [:span "there"]]]]
      (is (= h (adapt h)))))
  (testing "with hiccup containing views"
    (is (= [:div
            [:div {:silta-view-form "silta.adapter-test/va"
                   :silta-view-id "<id>"}
             "got: hello"]
            [:div {:silta-view-form "silta.adapter-test/vb"
                   :silta-view-id "<id>"}
             [:span 1 2]]]
           (mangle-attrs
            (adapt [:div
                    [va "hello"]
                    [vb 1 2]])))))
  (testing "with seq of hiccup"
    (let [hs `([:div [:span "what's"]]
               [:div [:span "up"]])]
      (is (= hs (adapt hs))))
    (is (= `([:div [:span "first"]]
             [:div
              [:div {:silta-view-form "silta.adapter-test/va"
                     :silta-view-id "<id>"}
               "got: hello"]])
           (mangle-attrs
            (adapt `([:div [:span "first"]]
                     [:div [va "hello"]]))))))
  (testing "with seq of forms"
    (is (= `((~'some-fn-call ~'x)
             nil
             [:div {:silta-view-form "silta.adapter-test/va"
                    :silta-view-id "<id>"}
              "got: hello, there"])
           (mangle-attrs
            (adapt `((~'some-fn-call ~'x)
                     nil
                     [va "hello, there"])))))))

;; TODO: (IMPORTANT) test that two invocations of same view yield identical attributes

(deftest test-process-form
  (testing "with let forms"
    (is (= (let [x 1] (silta.adapter/adapt ('fn-call x)))
           (process (let [x 1] ('fn-call x)))))))

(deftest test-process
  (testing "with nothing"
    (is (= nil (process nil))))
  (testing "with fully evaluated views, default mode"
    (is (= [:div "got: stuff"]
           (process [:div "got: stuff"])))
    (is (= [:div {:top "div"}
            [:div {:silta-view-form "silta.adapter-test/vb"
                   :silta-view-id "<id>"}
             [:span "hello" "there"]]]
           (mangle-attrs
            (process [:div {:top "div"}
                      [vb "hello" "there"]]))))
    (is (= [:div
            [:div
             {:silta-view-form "silta.adapter-test/va"
              :silta-view-id "<id>"}
             "got: stuff"]
            [:div
             {:silta-view-form "silta.adapter-test/va"
              :silta-view-id "<id>"}
             "got: and more"]]
           (mangle-attrs
            (process [:div
                      [va "stuff"]
                      [va "and more"]]))))
    (is (= [:div
            [:span {:silta-sink-id "<id>"
                    :silta-view-form "silta.adapter-test/vsink"}
             100]]
           (mangle-attrs
            (process [:div [vsink 100]])))))
  (testing "with fully evaluated views, view mode"
    (is (= [:div {:silta-view-form "my-view"}
            [:span "hello, there"]]
           (mangle-attrs
            (process {:view-name "my-view"}
                     [:div [:span "hello, there"]]))))))


