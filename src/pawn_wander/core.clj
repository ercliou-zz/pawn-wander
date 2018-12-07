(ns pawn-wander.core)

(def checkerboard-size 10)

(defn inside-checkerboard? [n [x y]]
  (and (>= x 0) (>= y 0) (< x n) (< y n)))

(defn unchunk [s]
  (lazy-seq
    (when-let [[x] (seq s)]
      (cons x (seq1 (rest s)))))) 

(defn vertical-horizontal-tiles [[x y]]
  [[(+ x 3) y]
   [(- x 3) y]
   [x (+ y 3)]
   [x (- y 3)]])

(defn diagonal-tiles [[x y]]
  [[(+ x 2) (+ y 2)]
   [(+ x 2) (- y 2)]
   [(- x 2) (+ y 2)]
   [(- x 2) (- y 2)]])

(defn reachable-tiles [n [x y :as pos]]
  (->> (concat (vertical-horizontal-tiles pos)
               (diagonal-tiles pos))
       (filter (partial inside-checkerboard? n))
       shuffle
       unchunk))

(defn all-visited? [n path] (= (count path) (* n n)))

(defn possible-tiles [n visited-positions]
  (->> (reachable-tiles n (last visited-positions))
       (remove (set visited-positions))))

(defn platform-graph [n]
  (fn [pos]
    (reachable-tiles n pos)))

(defn neighbors [graph pos] (graph pos))

(def my-graph (platform-graph 10))
(def neighbors00 (neighbors my-graph [0 0]))

(defn hamilton-path [n visited]
  (if (all-visited? n visited)
    visited
    (when-let [tiles-to-try (seq (possible-tiles n visited))]
      (->> tiles-to-try
           (map #(hamilton-path n (conj visited %)))
           (filter some?)
           first))))

(comment (hamilton-path 10 [[0 0]]))

