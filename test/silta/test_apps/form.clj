(ns silta.test-apps.form
  "A simple form app.
  A demonstration of data groups."
  (:require [silta.core :refer [defview]]
            [silta.html :refer [html]]))

;; TODO: handle input updates on-blur, etc.
;; -> re-run validation logic for group (e.g., form elements)
;; -> change submit "disabled" state

(defview summary
  [data-str]
  [:div {:id "form-summary"}
   (when data-str
     [:div
      [:p "You submitted:"]
      [:p {:id "data-string"} (str data-str)]])])

(defview form
  []
  [:form {:on-submit [[:swap {:target "#form-summary"}
                       [summary {:some-input [:value "#some-input"]
                                 :a-checkbox [:checked "#a-checkbox"]}]]]}
   [:div
    [:input {:type :text
             :id "some-input"
             :placeholder "Enter some text here"}]]
   [:div
    [:label
     [:input {:id "a-checkbox"
              :type :checkbox}]
     "Check me!"]]
   [:div
    [:input {:type :submit
             :value "Submit"}]]])

(def page
  (html
    [:div
     (form)
     (summary nil)
     (silta.core/make-script-tag)]))
