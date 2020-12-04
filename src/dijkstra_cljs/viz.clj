(ns dijkstra-cljs.viz
  (:require [rhizome.viz :refer [view-graph]]))

; Graphs are represented as maps with nodes as keys,
; each containing another map of its neighbors to their costs.

(def g {:1 {:2 1 :3 2}
        :2 {:4 4}
        :3 {:4 2}
        :4 {}})

(def demo-graph
  {:red    {:green 10, :blue   5, :orange 8}
   :green  {:red 10,   :blue   3}
   :blue   {:green 3,  :red    5, :purple 7}
   :purple {:blue 7,   :orange 2}
   :orange {:purple 2, :red    2}})

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

(defn show!
  "Generates and displays a GraphViz image of graph."
  [graph]
  (view-graph
   (keys graph) (fn [n] (map first (get graph n)))
   :node->descriptor (fn [n] {:label (name n)})
   :edge->descriptor (fn [src dst] {:label (dst (src graph))})))

(show! g)