(ns abc.extractors.ruby
    (:import (org.jrubyparser Parser CompatVersion)
           (org.jrubyparser.parser ParserConfiguration)
           (java.io StringReader)))

(defn parse-ruby [rb]
  (let [parser (Parser.)
        config (ParserConfiguration. 0 CompatVersion/RUBY1_9)]
    (.parse parser "" (StringReader. rb) config)))

(defn- children? [_] true)
(defn- children [n]
  (.childNodes n))
(defn- make-tree [root]
  (tree-seq children? children root))

(defn- defn-node? [n]
  (instance? org.jrubyparser.ast.DefnNode n))
(defn- assignment-node? [n]
  (instance? org.jrubyparser.ast.LocalAsgnNode n))
(defn- conditional-node? [n]
  (instance? org.jrubyparser.ast.IfNode n))
(defn- branch-node? [n]
  (instance? org.jrubyparser.ast.FCallNode n))

(defn- filter-nodes [root f]
  (loop [tree root]
    (cond (empty? tree) '()
          (f (first tree)) (cons (first tree) (filter-nodes (rest tree) f))
          (seq? (first tree)) (concat (filter-nodes (first tree) f)
                                      (filter-nodes (rest tree) f))
          :else (recur (rest tree)))))

(defn- line-number [n]
  (inc (.. n getPosition getEndLine)))

(defn- line-range [n]
  (let [source-position (.getPosition n)]
    [(.getStartOffset source-position)
     (.getEndOffset source-position)]))

(defn- extract [f]
  (fn [root]
    (let [nodes (filter-nodes (make-tree root) f)]
      (when-not (empty? nodes)
        (map (fn [n]
               {:line (line-number n) :range (line-range n)})
             nodes)))))

(defn- ruby-source [rb [start end]]
  (apply str (take (- end start) (drop start rb))))

(defn- with-source [rb & fks]
  (fn [root]
    (reduce (fn [e [f k]]
              (assoc e k
                (map (fn [env]
                       (assoc env :source (ruby-source rb (:range env))))
                     ((extract f) root))))
            {}
            fks)))

(defn parse [rb]
  (let [root (parse-ruby rb)
        defn-nodes (filter-nodes (make-tree root) defn-node?)]
    (map (with-source rb [assignment-node? :assignments]
                         [conditional-node? :conditionals]
                         [branch-node? :branches])
         defn-nodes)))
