(ns gremid.xml-test
  (:require
   [clojure.test :refer [deftest is]]
   [gremid.xml :as gxml])
  (:import
   (com.ctc.wstx.api WstxOutputProperties)
   (javax.xml.stream XMLInputFactory)
   (org.codehaus.stax2 XMLInputFactory2 XMLOutputFactory2)))

(def input-factory
  (doto ^XMLInputFactory2 (XMLInputFactory2/newInstance)
    (.configureForRoundTripping)
    (.setProperty XMLInputFactory/IS_NAMESPACE_AWARE false)))

(def output-factory
  (doto ^XMLOutputFactory2 (XMLOutputFactory2/newInstance)
    (.configureForXmlConformance)
    (.setProperty XMLOutputFactory2/XSP_NAMESPACE_AWARE false)
    (.setProperty WstxOutputProperties/P_USE_DOUBLE_QUOTES_IN_XML_DECL true)))

(def sample-xml
  (str "<?xml version=\"1.0\"?>"
       "<a b=\"c\" z:x=\"a\">dsf<!-- Kommentar -->xs<c/><![CDATA[blub]]></a>"))

(deftest round-tripping
  (binding [gxml/*input-factory* input-factory
            gxml/*output-factory* output-factory]
    (->>
     sample-xml
     (gxml/read-events)
     (gxml/events->node)
     (gxml/node->events)
     (gxml/write-events *out*)
     (with-out-str)
     (= sample-xml)
     (is))))

(def c-element?
  (comp (partial = :c) gxml/qname->kw gxml/->qname))

(deftest subtrees
  (binding [gxml/*input-factory*  input-factory
            gxml/*output-factory* output-factory]
    (->>
     sample-xml
     (gxml/read-events)
     (gxml/events->subtrees c-element?)
     (filter map?) (first) (:tag) (= :c)
     (is))))
