(ns clj-tiny-rb.core
  "A very basic red-black tree implementation of an Okasaki-style left leaning red black tree
   see: http://www.mew.org/~kazu/proj/red-black-tree/")

(defrecord Node [color left val right])

(defn red?
  [^Node node]
  (and node (= :red (.color node))))

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
  [color ^Node left val right]
  (if (and (= :black color)
           (red? left)
           (red? (.left left)))
    (let [lleft (.left left)
          lval (.val left)
          lright (.right left)]
      (red
        (make-black lleft)
        lval
        (black lright
               val
               right)))
    (->Node color left val right)))

(defn balancer
  [color left val ^Node right]
  (cond
    (and (= :black color)
         (red? left)
         (red? right)) (red (make-black left)
                            val
                            (make-black right))
    (red? right) (->Node color
                         (red left val (.left right))
                         (.val right)
                         (.right right))
    :else (->Node color left val right)))

(defn- insert*
  [^Node node val]
  (if-not node
    (red nil val nil)
    (case (compare val (.val node))
      -1 (balancel (.color node) (insert* (.left node) val) (.val node) (.right node))
      1 (balancer (.color node) (.left node) (.val node) (insert* (.right node) val))
      0 node)))

(defn insert
  ([node val]
   (make-black (insert* node val)))
  ([node val & vals]
   (reduce insert node (cons val vals))))

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
  [^Node node val]
  (when node
    (case (compare val (.val node))
      -1 (recur (.left node) val)
      1 (recur (.right node) val)
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
