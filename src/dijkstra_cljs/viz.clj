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

(defn add-random-node
  "Outputs a graph with a random edge added to it.
   Will not connect a node to itself, or a node
   that would result in a bidirected edge."
  [graph]
  (let [nodes (keys graph)
        rand-node (fn [nodes] (rand-nth nodes))
        node (rand-node nodes)
        neighbors (remove #(= node %) nodes)
        unconnected (remove #(contains? (% graph) node) neighbors)]
    (assoc-in graph [node (rand-nth unconnected)]
              (inc (rand-int 9)))))

(add-random-node (zipmap (nodes 6) (repeat {})))

; We want to reject {:3 5} being placed onto node :4
; because node :3 already has :4 in it.
; So we need to create a list of all the nodes that contain
; the target node and remove them.

(def m {:1 {}, :2 {}, :3 {:4 2}, :4 {:3 5 :2 5}, :5 {}, :6 {}})

(defn count-edges [graph]
  (reduce + (for [node graph]
              (count (val node)))))

(defn rand-graph [n s]
  (loop [graph (zipmap (nodes n) (repeat {}))]
         (if (< (count-edges graph) s)
           (recur (add-random-node graph))
           graph)))

(show! (rand-graph 6 6))

(def disconnected-graph
  {:1 {:4 5}, :2 {:5 1, :4 9}, :3 {}, :4 {}, :5 {:4 7}, :6 {:5 7, :1 2}})

; So, let's make a function that takes a node and a graph
; and outputs this list.

; It will take:
; {:1 {}, :2 {}, :3 {:4 2}, :4 {}, :5 {}, :6 {}}
; and :4
; And output :3

(def m {:1 {}, :2 {}, :3 {:4 2}, :4 {}, :5 {}, :6 {}})

(contains? (:3 m) :4)

(filter #(contains? (% m) :4) (keys m))

; Now similarly, we need to remove from the list of potential neighbors
; the keys that are already present

(->>
 (add-random-node (zipmap (nodes 6) (repeat {})))
 (show!))

(->>
 (nth (iterate add-random-node (zipmap (nodes 6) (repeat {}))) 2)
 (show!))

(show! g)

(Math/sqrt .57)