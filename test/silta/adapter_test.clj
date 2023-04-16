(ns silta.adapter-test
  (:require [clojure.test :refer [deftest is testing]]
            [silta.test-utils :refer [-process]]
            [silta.hiccup :as sh]
            [silta.core :refer [defview]]
            [silta.utils :refer [clj->json]]
            [silta.sources :refer [*mock-view-ids*]]
            [silta.adapter :refer [*compile-hiccup* process adapt adapt-attrs]]))

;; --- helpers ---

(defview ^:no-html va
  [x]
  [:div (format "got: %s" x)])

(defview ^:no-html vb
  [x y]
  [:div {:id "vb-root"}
   [:span x y]
   [:button {:class "btn btn-style"
             :on-click [[:swap {:target "#vb-root"}
                         [va "replaced!"]]]}
    "Click Me!"]])

(defview ^:no-html ^:sink vsink
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
  (binding [*compile-hiccup* false
            *mock-view-ids* true]
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
              [:div {:silta-view-name "silta.adapter-test/va"
                     :silta-view-type "view"
                     :silta-view-id "<id>"}
               "got: hello"]
              [:div {:id "vb-root"
                     :silta-view-name "silta.adapter-test/vb"
                     :silta-view-type "view"
                     :silta-view-id "<id>"}
               [:span 1 2]
               [:button {:class "btn btn-style"
                         :silta-events (clj->json {:click [[:swap {:target "#vb-root"}
                                                            ["/va" "replaced!"]]]})}
                "Click Me!"]]]
             (adapt [:div
                     [va "hello"]
                     [vb 1 2]]))))
    (testing "with seq of hiccup"
      (let [hs `([:div [:span "what's"]]
                 [:div [:span "up"]])]
        (is (= hs (adapt hs))))
      (is (= `([:div [:span "first"]]
               [:div
                [:div {:silta-view-name "silta.adapter-test/va"
                       :silta-view-type "view"
                       :silta-view-id "<id>"}
                 "got: hello"]])
             (adapt `([:div [:span "first"]]
                      [:div [va "hello"]])))))
    (testing "with seq of forms"
      (letfn [(test-fn [x] x)]
        (is (= `((test-fn ~'x)
                 nil
                 [:div {:silta-view-name "silta.adapter-test/va"
                        :silta-view-type "view"
                        :silta-view-id "<id>"}
                  "got: hello, there"])
               (adapt `((test-fn ~'x)
                        nil
                        [va "hello, there"]))))))))

(deftest test-process-form
  (testing "with let forms"
    (letfn [(tst-fn [x] x)]
      (is (= (let [x 1] (silta.adapter/adapt (tst-fn x)))
             (-process (let [x 1] (tst-fn x))))))))

(deftest test-process
  (testing "with nothing"
    (is (= nil (-process nil))))
  (testing "with fully evaluated views, default mode"
    (is (= [:div "got: stuff"]
           (-process [:div "got: stuff"])))
    (is (= [:div {:top "div"}
            [:div {:id "vb-root"
                   :silta-view-name "silta.adapter-test/vb"
                   :silta-view-type "view"
                   :silta-view-id "<id>"}
             [:span "hello" "there"]
             [:button {:class "btn btn-style"
                       :silta-events (clj->json {:click [[:swap {:target "#vb-root"}
                                                          ["/va" "replaced!"]]]})}
              "Click Me!"]]]
           (-process [:div {:top "div"}
                      [vb "hello" "there"]])))
    (is (= [:div
            [:div
             {:silta-view-name "silta.adapter-test/va"
              :silta-view-type "view"
              :silta-view-id "<id>"}
             "got: stuff"]
            [:div
             {:silta-view-name "silta.adapter-test/va"
              :silta-view-type "view"
              :silta-view-id "<id>"}
             "got: and more"]]
           (-process [:div
                      [va "stuff"]
                      [va "and more"]])))
    (is (= [:div
            [:span {:silta-view-id "<id>"
                    :silta-view-type "sink"
                    :silta-view-name "silta.adapter-test/vsink"}
             100]]
           (-process [:div [vsink 100]]))))
  (testing "with fully evaluated views, view mode"
    (is (= [:div {:silta-view-name "my-view"
                  :silta-view-type "view"
                  :silta-view-id "<id>"}
            [:span "hello, there"]]
           (-process {:view-sym 'my-view
                      :view-name "my-view"}
                     [:div [:span "hello, there"]]))))
  (testing "with equivalent view invocations -- should have equivalent attributes"
    (is (= (eval `(process {:no-html true} [va "test"]))
           (eval `(process {:no-html true} [va "test"]))))
    (is (= (eval `(process {:no-html true} [vsink 100]))
           (eval `(process {:no-html true} [vsink 100])))))
  (testing "with different view invocations -- should have different attributes"
    (let [attrs1 (sh/get-attrs (eval `(process {:no-html true} [va "test1"])))
          attrs2 (sh/get-attrs (eval `(process {:no-html true} [va "test2"])))]
      (is (not= attrs1 attrs2)))))


