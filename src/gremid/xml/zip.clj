(ns gremid.xml.zip
  (:refer-clojure :exclude [ancestors descendants])
  (:require [clojure.zip :as zip]))

(defn right-locs
  "Returns a lazy sequence of locations to the right of loc, starting with loc."
  [loc]
  (lazy-seq (when loc (cons loc (right-locs (zip/right loc))))))

(defn left-locs
  "Returns a lazy sequence of locations to the left of loc, starting with loc."
  [loc]
  (lazy-seq (when loc (cons loc (left-locs (zip/left loc))))))

(defn leftmost?
  "Returns true if there are no more nodes to the left of location loc."
  [loc]
  (nil? (zip/left loc)))

(defn rightmost?
  "Returns true if there are no more nodes to the right of location loc."
  [loc]
  (nil? (zip/right loc)))

(defn children
  "Returns a lazy sequence of all immediate children of location loc,
  left-to-right."
  [loc]
  (when (and loc (zip/branch? loc)) (right-locs (zip/down loc))))

(defn descendants
  "Returns a lazy sequence of all descendants of location loc, in
  depth-first order, left-to-right, starting with loc."
  [loc]
  (lazy-seq (when loc (cons loc (mapcat descendants (children loc))))))

(defn subtree
  "All descendants of loc, excluding loc itself."
  [loc]
  (seq (rest (descendants loc))))

(defn ancestors
  "Returns a lazy sequence of all ancestors of location loc, starting
  with loc and proceeding to loc's parent node and on through to the
  root of the tree."
  [loc] (lazy-seq (when loc (cons loc (ancestors (zip/up loc))))))

(defn ancestry
  "All ancestors of loc, excluding loc itself."
  [loc]
  (seq (rest (ancestors loc))))

(defn prev-locs
  "Returns a lazy sequence of locations preceding and starting with loc."
  [loc]
  (lazy-seq (when loc (cons loc (prev-locs (zip/prev loc))))))

(defn next-locs
  "Returns a lazy sequence of locations following and starting with loc."
  [loc]
  (lazy-seq (when (and loc (not (zip/end? loc)))
              (cons loc (next-locs (zip/next loc))))))
