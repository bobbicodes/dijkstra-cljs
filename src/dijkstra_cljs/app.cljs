(ns dijkstra-cljs.app
  (:require [reagent.core :as r]))

(defonce step (r/atom 0))

(def g {:1 {:2 1 :3 2}
        :2 {:4 4}
        :3 {:4 2}
        :4 {}})

(defonce graph-db (r/atom {}))

(defn init-graph! [graph initial-node]
  (swap! graph-db assoc :nodes (zipmap (keys graph)
                                       (repeat {:distance 99999999
                                                :parent nil})))
  (swap! graph-db assoc :unvisited (set (keys graph)))
  (swap! graph-db assoc-in [:nodes initial-node :distance] 0)
  (swap! graph-db assoc :current-node initial-node))

(init-graph! g :1)

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

@step

(defn app []
  [:div#app
   [:center
    [:h1 "Dijkstra's Algorithm"]
    [:p (str g)]
    [:p (str @graph-db)]
    [:button
     {:on-click
      (fn step-click [e]
        (swap! step inc))}
     "Next"]
    [:button
     {:on-click
      (fn step-click [e]
        (reset! step 0)
        (init-graph! g :1))}
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
