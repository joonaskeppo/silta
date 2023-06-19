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
  (if checked
    [:s text]
    [:span text]))

(defview todo-list
  "Show list of all (removable) todos"
  [todos]
  [:ul#todo-list
   ;; TODO: add unit test for this kind of `for` wrappage case
   (for [idx (range (count todos))
         :let [item-id (str "todo-" (inc idx))
               {:keys [text checked]} (get todos idx)]]
     [:li {:id item-id}
      [todo-item {:text text :checked checked}]
      [:button {:data-test "remove-todo-button"
                ;; FIXME: something wrong?
                :on-click [[:remove {:target (str "#" item-id)}]]}
       "Remove"]])])

(comment
  ((:renderer todo-item) {:params {:text "hello" :checked true}})
  ((:renderer todo-list) {:params [[{:text "hello" :checked true}]]}))

(defview add-todo
  "Input field to add a new todo item"
  []
  [:div
   [:label "Next todo:"
    [:input {:type "text"
             :id "new-todo"
             :data-test "add-todo-input"}]]
   [:button {:on-click [[:prepend {:target "#todo-list:first-child"}
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
