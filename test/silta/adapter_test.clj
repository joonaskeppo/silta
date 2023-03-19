(ns silta.adapter-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [silta.test-utils :refer [mangle-attrs]]
            [silta.hiccup :as sh]
            [silta.core :refer [defview]]
            [silta.adapter :refer [process adapt]]))

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

;; TODO: test-adapt

;; TODO: (IMPORTANT) test that two invocations of same view yield identical attributes

(deftest test-process-form
  (testing "with let forms"
    (is (= (let [x 1] (silta.adapter/adapt ('fn-call x)))
           (process (let [x 1] ('fn-call x)))))))

(deftest test-process
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


