# Problems and Their Potential Solutions

## Problem: Component Rendering

The current approach is fairly macro-heavy,
and it's likely that some things don't work as expected.
There's a certain "magical" feel (in a bad way) to the way
top-level page `def`s look.
The "component-ness" is implied by the structure.
Here's the top-level `page` for the todo app (in the tests folder):
```clj
(def page
  (let [initial-counter 0]
    [:div
     [intro-text]
     [test-sink +example-source+]
     [notice {:counter initial-counter}]
     [button initial-counter]]))
```
This feels like a violation of the principle of least surprise waiting to happen.
What happens if you wrap the thing in Hiccup's `html` macro?
It fails to render completely as Hiccup doesn't understand `intro-text`, `test-sink`, `notice`, and `button`
in the context of the library.

A better approach should be less surprising, like so:
```clj
(def page
  (let [initial-counter 0]
    [:div
     (intro-text)
     (test-sink +example-source+)
     (notice {:counter initial-counter})
     (button initial-counter)]))
```
In this version, the vars would be bound to the renderer.
`(intro-text)` would be equivalent to `((:renderer intro-text))` in the current version.

### Solution?

The path of least resistance here would be to implement IFn
for View records.
This would make the "syntax-sugared" version possible.
Instead of `((:renderer intro-text))`, you could simply invoke `(intro-text)`.

As a result, there should no longer be a need to invoke renderers
within the top-level `page`.
Now it's just hiccup in a sea of hiccup, and we're a tiny bit closer to
reifing a seamless htmx-like layer on top of regular ole' hiccup!


## Problem: `after`, `before` vs Middleware

While `before` (in particular) was useful for debugging early on,
it's obvious that regular Ring middleware is far more useful and preferable
in a production system.

In a production setting, it is conceivable that, for instance, one set of routes (`a`) has middleware stack `ma`,
another (`b`) has stack `mb`, and certain individual routes have some variation of stack `mc` on top of `ma` or `mb`.

### Solution?

Supposing we can isolate all of our Silta routes to `/silta/**`, we could have something like:

```clj
[["/silta" 
 (make-routes:a)
 (make-routes:b)]]
```

where `make-routes:a` and `make-routes:b` are fns creating *all* routes for a and b, respectively.
This means something like:

```clj
(defn make-routes:a []
 ["/a"
  ["/" ...]
  ["/some-static-route" ...]

  ;; `silta/make-view-routes` creates a base Reitit route structure for all matched views.
  ;; This base structure gets merged deeply with child overrides.
  ;; In other words, if the base structure has `["/some-view" {:handler ... :parameters ...}]`
  ;; and the override has `["/some-view" {:middleware mc}]`, the resultant route would be
  ;; `["/some-view" {:handler ... :parameters ... :middleware mc}]`.

  (silta/make-view-routes
   ["" {:middleware                 (into silta/default-view-middleware ma) ; override default `:middleware`
        ; :silta/extra-middleware     ma ; the alternative approach
        :silta/extra-views          #{some.secret/view}
        :silta/exclude-views        #{a.other.views/test-view-please-ignore}
        :silta/exclude-namespaces   #{'b.views} ; has no effect here: (set/difference included-namespaces excluded-namespaces)
        :silta/include-namespaces   #{'a.some.views 'a.other.views}}
    [a.some.views/my-view {:middleware mc}]])])
```
