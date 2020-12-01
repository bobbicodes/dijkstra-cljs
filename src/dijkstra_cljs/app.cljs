(ns dijkstra-cljs.app
  (:require [reagent.core :as r]))

(defonce step (r/atom 0))

(def demo-graph
  {:red    {:green 10, :blue   5, :orange 8}
   :green  {:red 10,   :blue   3}
   :blue   {:green 3,  :red    5, :purple 7}
   :purple {:blue 7,   :orange 2}
   :orange {:purple 2, :red    2}})

(def g {:1 {:2 1 :3 2}
        :2 {:4 4}
        :3 {:4 2}
        :4 {}})

(def computerphile
  {:s {:a 7 :b 2 :c 3}
   :a {:s 7 :b 3 :d 4}
   :b {:s 2 :a 3 :d 4 :h 1}
   :c {:s 3 :l 2}
   :d {:a 4 :b 4 :f 5}
   :e {:g 2 :k 5}
   :f {:d 5 :h 3}
   :g {:h 2 :e 2}
   :h {:b 1 :f 3 :g 2}
   :i {:l 4 :j 6 :k 4}
   :j {:i 6 :l 4 :k 4}
   :k {:i 4 :j 4 :e 5}
   :l {:c 2 :i 4 :j 4}})

(defonce graph (r/atom computerphile))
(defonce starting-node (r/atom :s))
(defonce graph-db (r/atom {}))

(defn init-graph! [graph initial-node]
  (swap! graph-db assoc :nodes (zipmap (keys graph)
                                       (repeat {:distance 99999999
                                                :parent nil})))
  (swap! graph-db assoc :unvisited (set (keys graph)))
  (swap! graph-db assoc-in [:nodes initial-node :distance] 0)
  (swap! graph-db assoc :current-node initial-node))

(init-graph! @graph @starting-node)

(defn update-node! [node graph]
  (let [edge-dist (node ((:current-node @graph-db) graph))]
    (when (< edge-dist (:distance (node (:nodes @graph-db))))
      (swap! graph-db assoc-in [:nodes node :parent] (:current-node @graph-db))
      (swap! graph-db assoc-in [:nodes node :distance]
             (+ (if (:parent (node (:nodes @graph-db)))
                  (:distance ((:parent (node (:nodes @graph-db))) (:nodes @graph-db)))
                  0)
                edge-dist)))))

(defn next-node [graph]
  (key (first (filter #(pos? (get-in (val %) [:distance]))
                      (sort-by #(get-in (val %) [:distance])
                               (select-keys (:nodes graph) (:unvisited graph)))))))

(defn mark-visited! []
  (swap! graph-db assoc :unvisited (disj (:unvisited @graph-db) (:current-node @graph-db)))
  (swap! graph-db assoc :current-node (next-node @graph-db)))

(defn app []
  [:div#app
   [:center
    [:h1 "Dijkstra's Algorithm"]
    [:p (str @graph)]
    [:p (str (:nodes @graph-db))]
    [:p (str "Unvisited: " (:unvisited @graph-db))]
    [:p (str "Current node: " (:current-node @graph-db))]
    [:button
     {:on-click
      (fn step-click [e]
        (run! #(update-node! % @graph) (keys ((:current-node @graph-db) @graph)))
        (mark-visited!)
        (swap! step inc))}
     "Next"]
    [:button
     {:on-click
      (fn step-click [e]
        (reset! step 0)
        (init-graph! @graph @starting-node))}
     "Reset"]
    [:p (str "Step " @step)]]])

(defn render []
  (r/render [app]
            (.getElementById js/document "root")))

(defn ^:dev/after-load start []
  (render)
  (js/console.log "start"))

(defn ^:export init []
  (js/console.log "init")
  (start))

(defn ^:dev/before-load stop []
  (js/console.log "stop"))
