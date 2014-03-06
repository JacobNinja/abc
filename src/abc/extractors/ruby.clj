(ns abc.extractors.ruby
    (:import (org.jrubyparser Parser CompatVersion)
           (org.jrubyparser.parser ParserConfiguration)
           (java.io StringReader)))

; AST walking

(defn- children? [_] true)

(defn- children [n]

  (.childNodes n))
(defn- make-tree [root]
  (tree-seq children? children root))

(defn- filter-nodes [root f]
  (loop [tree root]
    (cond (empty? tree) '()
          (f (first tree)) (cons (first tree) (filter-nodes (rest tree) f))
          (seq? (first tree)) (concat (filter-nodes (first tree) f)
                                      (filter-nodes (rest tree) f))
          :else (recur (rest tree)))))

; Node types

(defn- type-of? [n & types]
  (some (fn [type]
          (instance? type n))
        types))

(defn- defn-node? [n]
  (instance? org.jrubyparser.ast.DefnNode n))

(defn- assignment-node? [n]
  (type-of? n
            org.jrubyparser.ast.LocalAsgnNode
            org.jrubyparser.ast.InstAsgnNode))

(defn- conditional-node? [n]
  (type-of? n
            org.jrubyparser.ast.IfNode
            org.jrubyparser.ast.CaseNode))

(defn- branch-node? [n]
  (instance? org.jrubyparser.ast.FCallNode n))

; Source parsing

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

(defn parse-ruby [rb]
  (let [parser (Parser.)
        config (ParserConfiguration. 0 CompatVersion/RUBY1_9)]
    (.parse parser "" (StringReader. rb) config)))

(defn parse [rb]
  (let [root (parse-ruby rb)
        defn-nodes (filter-nodes (make-tree root) defn-node?)]
    (map (with-source rb [assignment-node? :assignments]
                         [conditional-node? :conditionals]
                         [branch-node? :branches])
         defn-nodes)))

;(def ternary (first (rest (rest (make-tree (parse-ruby "foo ? true : false"))))))
;(def if-node (first (rest (rest (make-tree (parse-ruby "if foo then true else false end"))))))

(parse-ruby "@bar = 3")
