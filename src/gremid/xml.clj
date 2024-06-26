(ns gremid.xml
  (:require
   [clojure.string :as str])
  (:import
   (java.io File InputStream OutputStream Reader StringReader Writer)
   (java.nio.file Path)
   (java.net URI URL)
   (java.util Collections)
   (javax.xml XMLConstants)
   (javax.xml.namespace QName)
   (javax.xml.stream XMLEventFactory XMLEventReader XMLEventWriter XMLInputFactory XMLOutputFactory XMLStreamConstants)
   (javax.xml.stream.events Attribute Characters Comment ProcessingInstruction StartElement XMLEvent)
   (javax.xml.transform Result Source)
   (javax.xml.transform.dom DOMResult DOMSource)
   (javax.xml.transform.stream StreamResult StreamSource)
   (org.w3c.dom Node)
   (org.xml.sax InputSource)))

(defprotocol IO
  (as-source [v])
  (as-input-source [v])
  (as-result [v])
  (close! [v]))

(extend-protocol IO
  Source
  (as-source [^Source v] v)
  (close! [_])

  Result
  (as-result [^Result v] v)
  (close! [_])

  Node
  (as-source [^Node v] (DOMSource. v))
  (as-result [^Node v] (DOMResult. v))
  (close! [_])

  File
  (as-source [^File v] (StreamSource. v))
  (as-input-source [^File v] (as-input-source (.toURI v)))
  (close! [_])

  Path
  (as-source [^Path v] (as-source (.toFile v)))
  (as-input-source [^Path v] (as-input-source (.toFile v)))
  (close! [_])

  URI
  (as-source [^URI v] (StreamSource. (str v)))
  (as-input-source [^URI v] (InputSource. (str v)))
  (close! [_])

  URL
  (as-source [^URL v] (as-source (.toURI v)))
  (as-input-source [^URL v] (as-input-source (.toURI v)))
  (close! [_])

  InputStream
  (as-source [^InputStream v] (StreamSource. v))
  (as-input-source [^InputStream v] (InputSource. v))
  (close! [^InputStream v] (.close v))

  OutputStream
  (as-result [^OutputStream v] (StreamResult. v))
  (close! [^OutputStream v] (.close v))

  Reader
  (as-source [^Reader v] (StreamSource. v))
  (as-input-source [^Reader v] (InputSource. v))
  (close! [^Reader v] (.close v))

  Writer
  (as-result [^Writer v] (StreamResult. v))
  (close! [^Writer v] (.close v))

  String
  (as-source [^String v] (as-source (StringReader. v)))
  (as-input-source [^String v] (as-input-source (StringReader. v)))
  (close! [_]))

(defprotocol XMLNamed
  (->qname [v]))

(extend-protocol XMLNamed
  StartElement
  (->qname [v] (.getName v))

  XMLEvent
  (->qname [_] nil))


(def ^:dynamic ^XMLInputFactory *input-factory*
  (doto (XMLInputFactory/newInstance)
    (.setProperty XMLInputFactory/IS_NAMESPACE_AWARE false)))

(defn event-reader ^XMLEventReader
  [input]
  (.createXMLEventReader *input-factory* ^Source (as-source input)))

(defn read-events
  [input]
  (iterator-seq (event-reader input)))

(defn qname->kw
  [^QName qn]
  (let [prefix (not-empty (.getPrefix qn))]
    (->>
     (cond->> (.getLocalPart qn) prefix (str prefix ":"))
     (symbol)
     (clojure.lang.Keyword/intern))))

(defn attrs->map
  [^StartElement event]
  (persistent!
   (reduce
    (fn [m ^Attribute attr]
      (assoc! m (qname->kw (.getName attr)) (.getValue attr)))
    (transient (array-map))
    (iterator-seq (.getAttributes event)))))

(defn event->node
  [^StartElement event]
  (let [attrs (attrs->map event)]
    {:tag   (qname->kw (->qname event))
     :attrs (when (seq attrs) attrs)}))

(defn append-child!
  [current child]
  (vswap! current update :content (fnil conj []) child))

(defn append-text!
  [current char-buf]
  (let [^StringBuilder sb @char-buf]
    (when (pos? (.length sb))
      (append-child! current (.toString sb))
      (vreset! char-buf (StringBuilder.)))))

(def document-node
  {:tag :<< :attrs nil})

(defn events->node
  [events]
  (let [stack         (volatile! nil)
        current       (volatile! document-node)
        char-buf      (volatile! (StringBuilder.))
        append-child! (partial append-child! current)
        append-text!  (partial append-text! current char-buf)]
    (loop [events events]
      (if-let [^XMLEvent event (first events)]
        (do
          (condp = (.getEventType event)
            XMLStreamConstants/START_ELEMENT
            (do
              (append-text!)
              (vswap! stack conj @current)
              (vreset! current (event->node event)))
            XMLStreamConstants/END_ELEMENT
            (do
              (append-text!)
              (let [el @current]
                (vreset! current (peek @stack))
                (append-child! el)
                (vswap! stack pop)))
            XMLStreamConstants/PROCESSING_INSTRUCTION
            (let [^ProcessingInstruction pi event]
              (append-text!)
              (append-child! {:tag   :<?
                              :attrs {:target (.getTarget pi)
                                      :data   (.getData pi)}}))
            XMLStreamConstants/COMMENT
            (let [^Comment comment event]
              (append-text!)
              (append-child! {:tag   :<!
                              :attrs {:text (.getText comment)}}))
            XMLStreamConstants/CDATA
            (do
              (append-text!)
              (append-child! {:tag     :<c
                              :content (list (.getData ^Characters event))}))
            XMLStreamConstants/CHARACTERS
            (.append ^StringBuilder @char-buf (.getData ^Characters event))
            ;; skip
            nil)
          (recur (rest events)))
        (let [node @current]
          (if (and (= :<< (:tag node)) (not (second (:content node))))
            (first (:content node))
            node))))))

(defn traverse
  [node]
  (tree-seq :content :content node))

(defn tag-pred
  [tag]
  (comp (partial = tag) :tag))

(defn elements
  [tag node]
  (filter (tag-pred tag) (traverse node)))

(defn element
  [tag node]
  (first (elements tag node)))

(defn normalize-ws
  [s]
  (some-> s str/trim (str/replace #"\s+" " ") not-empty))

(defn text-content
  [node]
  (str/join (filter string? (traverse node))))

(defn texts
  [node]
  (some-> node text-content normalize-ws list))

(defn text
  [node]
  (first (texts node)))

(defn attrs
  [k node]
  (some-> node :attrs k list))

(defn attr
  [k node]
  (first (attrs k node)))

(def ^:dynamic ^XMLEventFactory event-factory
  (XMLEventFactory/newInstance))

(defn attr->event
  [[k v]]
  (.createAttribute event-factory (name k) (str v)))

(def ^XMLEvent start-document-event
  (.createStartDocument event-factory))

(def ^XMLEvent end-document-event
  (.createEndDocument event-factory))

(defn node->start-event
  [{:keys [tag attrs]}]
  (.createStartElement event-factory
                       XMLConstants/DEFAULT_NS_PREFIX
                       XMLConstants/NULL_NS_URI
                       (name tag)
                       (.iterator ^Iterable (into [] (map attr->event) attrs))
                       (.iterator ^Iterable (Collections/emptyList))))

(defn node->end-event
  [{:keys [tag]}]
  (.createEndElement event-factory
                     XMLConstants/DEFAULT_NS_PREFIX
                     XMLConstants/NULL_NS_URI
                     (name tag)))

(defn node->events
  [node]
  (if (string? node)
    (list (.createCharacters event-factory ^String node))
    (condp = (:tag node)
      :<c (list (.createCData event-factory ^String (text-content node)))
      :<! (list (.createComment event-factory ^String (attr :text node)))
      :<? (list (.createProcessingInstruction event-factory
                                              ^String (attr :target node)
                                              ^String (attr :data node)))
      :<< (concat [start-document-event]
                  (mapcat node->events (:content node))
                  [end-document-event])
      (concat [(node->start-event node)]
              (mapcat node->events (:content node))
              [(node->end-event node)]))))

(def ^:dynamic ^XMLOutputFactory *output-factory*
  (XMLOutputFactory/newInstance))

(defn event-writer ^XMLEventWriter
  [output]
  (.createXMLEventWriter *output-factory* ^Result (as-result output)))

(defn write-events
  [output events]
  (let [writer (event-writer output)
        doc?   (.isStartDocument ^XMLEvent (first events))]
    (when-not doc? (.add ^XMLEventWriter writer start-document-event))
    (doseq [event events] (.add ^XMLEventWriter writer ^XMLEvent event))
    (when-not doc? (.add ^XMLEventWriter writer end-document-event))))

(defn write-node
  [output node]
  (write-events output (node->events node)))

(comment
  (->>
   "<a b=\"c\" z:x=\"a\">dsf<!-- Kommentar -->xs<c/><![CDATA[blub]]></a>"
   (read-events)
   (events->node)
   (node->events)
   (write-events *out*)
   (with-out-str)
   (time)))

(defprotocol AsElements
  (sexp->nodes [expr]))

(extend-protocol AsElements
  clojure.lang.IPersistentVector
  (sexp->nodes [v]
    (let [[tag & content]       v
          [attrs & after-attrs] content
          [attrs content]       (if (map? attrs)
                                  [attrs after-attrs]
                                  [{} content])]
      (list {:tag     tag
             :attrs   attrs
             :content (mapcat sexp->nodes content)})))

  clojure.lang.ISeq
  (sexp->nodes [s] (mapcat sexp->nodes s))

  clojure.lang.Keyword
  (sexp->nodes [k]
    [{:tag     k
      :attrs   {}
      :content (list)}])

  java.lang.String
  (sexp->nodes [s] (list s))

  nil
  (sexp->nodes [_] nil)

  java.lang.Object
  (sexp->nodes [o] (list (str o))))

(defn sexp->node
  [v]
  (first (sexp->nodes v)))

(defn events->subtrees
  ([start? events]
   (events->subtrees events->node start? events))
  ([build start? events]
   (events->subtrees build start? events 0 []))
  ([build start? events depth subtree]
   (when-let [^XMLEvent evt (first events)]
     (let [events   (rest events)
           subtree? (pos? depth)]
       (if subtree?
         (let [subtree (conj subtree evt)
               depth   (cond-> depth (.isStartElement evt) inc (.isEndElement evt) dec)
               end?    (zero? depth)]
           (lazy-cat
            (when end? (list (build subtree)))
            (events->subtrees build start? events depth (if end? [] subtree))))
         (lazy-seq
          (if (and (.isStartElement evt) (start? evt))
            (events->subtrees build start? events 1 (conj subtree evt))
            (cons evt (events->subtrees build start? events depth subtree)))))))))
