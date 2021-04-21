;; https://developer.mozilla.org/en-US/docs/Web/API/NamedNodeMap
(defn attrs [node]
  (let [m (.-attributes node)
        len (.-length m)]
    (when (pos? len)
      (->> (range len)
           (map #(let [i (.item m %)]
                   [(-> i .-name keyword) (.-value i)]))
           (into {})))))

;; https://developer.mozilla.org/en-US/docs/Web/API/Node/nodeType#node_type_constants
(defn dom->hiccup [node]
  (condp = (.-nodeType node)
    js/Node.ELEMENT_NODE
    (let [tag (-> node .-tagName .toLowerCase keyword)
          attrs (attrs node)
          children (map dom->hiccup (.-childNodes node))]
      (->> (list* tag attrs children)
           (remove nil?)
           vec))

    js/Node.TEXT_NODE
    (.-nodeValue node)))
