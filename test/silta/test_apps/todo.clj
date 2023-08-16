(ns silta.test-apps.todo
  "A barebones todo app"
  (:require [silta.core :refer [defview make-routes]]
            [aleph.http :as http]
            [reitit.ring :as ring]
            [mount.core :as mount]))


;; use `def`-compliant notation: `todo:item`, `todo:list`
;; => `/todo/item`, `/todo/list`

(defview todo-item
  [{:keys [text checked]}]
  (let [item-id (str "todo-" (random-uuid))]
    [:li {:id item-id}
     (if checked
       [:s text]
       [:span text])
     [:button {:data-test "remove-todo-button"
               ;; FIXME: something wrong?
               :on-click [[:remove {:target (str "#" item-id)}]]}
      "Remove"]]))

(defview todo-list
  "Show list of all (removable) todos"
  [todos]
  [:ul#todo-list
   ;; TODO: add unit test for this kind of `for` wrappage case
   (for [todo todos]
     [todo-item todo])])

(defview add-todo
  "Input field to add a new todo item"
  []
  [:div
   [:label "Next todo:"
    [:input {:type "text"
             :id "new-todo"
             :data-test "add-todo-input"}]]
   [:button {:on-click [[:prepend {:target "#todo-list"}
                         [todo-item {:text [:value "#new-todo"]
                                     :checked false}]]]}
    "Add"]])

(def page
  (let [initial-todos [{:text "Make todo app" :checked true}
                       {:text "Make it pretty" :checked false}]]
    [:div
     [:h1 "Todos"]
     [add-todo]
     [todo-list initial-todos]]))

(comment
  ((:renderer add-todo) {:params []})
  ((:renderer todo-list) {:params [[{:text "Make todo app" :checked true}
                                    {:text "Make it pretty" :checked false}]]}))
