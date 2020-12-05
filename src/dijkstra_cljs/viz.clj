(ns dijkstra-cljs.viz
  (:require [rhizome.viz :refer [view-graph]]))

; Graphs are represented as maps with nodes as keys,
; each containing another map of its neighbors to their costs.

(def g {:1 {:2 1 :3 2}
        :2 {:4 4}
        :3 {:4 2}
        :4 {}})

(defn show!
  "Generates and displays a GraphViz image of graph."
  [graph]
  (prn graph)
  (view-graph
   (keys graph) (fn [n] (map first (get graph n)))
   :node->descriptor (fn [n] {:label (name n)})
   :edge->descriptor (fn [src dst] {:label (dst (src graph))})))

(defn nodes
  "Generates a list of n nodes as integer keywords."
  [n]
  (map #(keyword (str %)) (range 1 (inc n))))

(nodes 6)

(take 17 (repeatedly #(rand-nth (nodes 6))))

(zipmap (nodes 6) (repeat {}))

(defn add-random-node [graph]
  (let [nodes (keys graph)
        rand-node (fn [nodes] (first (shuffle nodes)))
        node (rand-node nodes)
        neighbors (remove #(= node %) nodes)]
    (assoc-in graph [node (rand-nth neighbors)]
              (inc (rand-int 9)))))

(remove #(= :2 %) (keys (zipmap (nodes 6) (repeat {}))))

(->>
 (add-random-node (zipmap (nodes 6) (repeat {})))
 (show!))

(->>
 (nth (iterate add-random-node (zipmap (nodes 6) (repeat {}))) 2)
 (show!))

(show! g)