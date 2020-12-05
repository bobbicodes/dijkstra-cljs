(ns dijkstra-cljs.random)

(def g {:1 {:2 1 :3 2}
        :2 {:4 4}
        :3 {:4 2}
        :4 {}})

(defn rand-weight
  "Generates a random integer from 1 to n."
  [n]
  (inc (rand-int n)))

(defn nodes
  "Generates a list of n nodes as integer keywords."
  [n]
  (map #(keyword (str %)) (range 1 (inc n))))

(defn possible-neighbors
  "Generates an infinite list of nodes in random order."
  [nodes]
  (repeatedly #(rand-nth nodes)))

(take 17 (repeatedly #(rand-nth (nodes 6))))

(take 17 (possible-neighbors (nodes 6)))

(defn add-random-node [graph]
  (let [nodes (keys graph)
        rand-node (fn [nodes] (first (shuffle nodes)))]
    (assoc-in graph [(rand-node nodes) (rand-node nodes)]
              (rand-weight 9))))

(zipmap (nodes 6) (repeat {}))

(add-random-node (zipmap (nodes 6) (repeat {})))

(show! (nth (iterate add-random-node (zipmap (nodes 6) (repeat {}))) 2))

(comment
  (take 5 (possible-neighbors (nodes 7))))

(defn rand-edges
  "Generates a random list of possible edges for a node,
   given a list of nodes."
  [nodes]
  (take (rand-int (count nodes)) (shuffle nodes)))

(comment
  (rand-edges (nodes 6)))

(defn assign-weights [edges]
  (zipmap
   edges
   (repeatedly #(rand-weight 9))))

(defn rand-graph [n s]
  (zipmap
   (nodes n)
   (map assign-weights (repeatedly n #(rand-edges (nodes n))))))

(comment
  (map assign-weights (repeatedly 4 #(rand-edges (nodes 4))))
  (repeatedly 4 #(rand-edges (nodes 4))))

;node can not be connected to itself - removes self-cycling
(def gg
  (let [graph (rand-graph 4 3)]
    (zipmap
     (keys graph)
     (for [[node edges] graph]
       (dissoc edges node)))))

(comment
  (zipmap
   (keys gg)
   (for [[node edges] gg]
     (dissoc edges node)))
  (show! gg))

(def G {:1 [:2 :3]
        :2 [:4]
        :3 [:4]
        :4 []})

(defn seq-graph [d g s]
  ((fn rec-seq [explored frontier]
     (lazy-seq
      (if (empty? frontier)
        nil
        (let [v (peek frontier)
              neighbors (g v)]
          (cons v (rec-seq
                   (into explored neighbors)
                   (into (pop frontier) (remove explored neighbors))))))))
   #{s} (conj d s)))

(comment
  ((fn rec-seq [explored frontier]
     (lazy-seq
      (if (empty? frontier)
        nil
        (let [v (peek frontier)
              neighbors (g v)]
          (cons v (rec-seq
                   (into explored neighbors)
                   (into (pop frontier) (remove explored neighbors))))))))
   #{s} (conj [] s)))

(def seq-graph-dfs (partial seq-graph []))
(def seq-graph-bfs (partial seq-graph (clojure.lang.PersistentQueue/EMPTY)))

(show! g)

(seq-graph-dfs (zipmap
                (keys g)
                (map keys (vals g))) :1) ; => (:1 :3 :4 :2)
(seq-graph-bfs G :1) ; => (:1 :2 :3 :4)
