(ns clj-tiny-rb.core
  "A very basic red-black tree implementation of an Okasaki-style left leaning red black tree
   see: http://www.mew.org/~kazu/proj/red-black-tree/
   - clearly a couple of orders of magnitude less efficient to create than clojure's sorted sets")

(defrecord Node [color left val right])

(defn red?
  [node]
  (= :red (:color node)))

(defn black?
  [node]
  (not (red? node)))

(defn red
  [left val right]
  (->Node :red left val right))

(defn black
  [left val right]
  (->Node :black left val right))

(defn make-black
  [node]
  (assoc node :color :black))

(defn balancel
  [{:keys [left val right] :as node}]
  (if (and (black? node)
           (red? left)
           (red? (:left left)))
    (let [lleft (:left left)
          lval (:val left)
          lright (:right left)]
      (red
        (make-black lleft)
        lval
        (black lright
               val
               right)))
    node))

(defn balancer
  [{:keys [color left val right] :as node}]
  (cond
    (and (black? node)
         (red? left)
         (red? right)) (red (make-black left)
                            val
                            (make-black left))
    (red? right) (->Node color
                         (red left val (:left right))
                         (:val right)
                         (:right right))
    :else node))

(declare insert-seq)


(defn insert
  ([node val]
   (cond
     (nil? node) (red nil val nil)
     :else
     (case (compare val (:val node))
       -1 (balancel (assoc node :left (insert (:left node) val)))
       1 (balancer (assoc node :right (insert (:right node) val)))
       0 node)))
  ([node val & vals]
   (insert-seq node (cons val vals))))

(defn insert-seq
  [rb coll]
  (reduce insert rb (seq coll)))

(defn rb->seq
  [node]
  (if (nil? node)
    nil
    (lazy-cat (rb->seq (:left node))
              (cons (:val node) nil)
              (rb->seq (:right node)))))

(defn find-val
  [node val]
  (when node
    (case (compare val (:val node))
      -1 (recur (:left node) val)
      1 (recur (:right node) val)
      0 val)))

(deftype Tree [node]
  clojure.lang.IPersistentSet
  (cons [this val] (Tree. (insert node val)))
  (empty [this] (Tree. nil))
  (equiv [this o] (= this o))
  (seq [this] (rb->seq node))
  (get [this val] (find-val node val))
  (contains [this val] (boolean (find-val node val))))

(defn seq->tree
  [coll]
  (Tree. (insert-seq nil coll)))

(defn tree
  [& vals]
  (seq->tree vals))
