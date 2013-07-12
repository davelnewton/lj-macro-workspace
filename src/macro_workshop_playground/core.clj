(ns macro-workshop-playground.core
  (:require clojure.walk)
  [:use clojure.tools.macro [clojure.walk :as walk]])
(def ... '())


;;;;
;;;; Funhouse
;;;;

(def opmap '{ + -, - +, * /, / * })

 
(defn funhouse [forms] 
  (let [sym (first forms)]
    (conj (rest forms) (opmap (first forms)))))

(defn funhouse [expr]
  (if (list? expr)
    (map funhouse expr)
    (or ('{ + -, - +, * /, / * } expr) expr)))

;;;;
;;;; Macros
;;;;
 
(defmacro in-funhouse [forms] 
  (funhouse forms))

(defmacro do-funhouse [& forms] 
  (list 'map 'funhouse (list 'quote forms)))

(defmacro do-funhouse [& forms] 
  (list 'do
    (list 'quote (map funhouse forms))))

(defmacro do-funhouse [& forms] 
  (cons 'do (map funhouse forms)))

;;;;
;;;; Syntax-quote
;;;;

(defmacro debug [expr]
  ...)

(defmacro debug [expr]
  (let [i# expr]
  (list 'do
    (list 'println
      (list 'quote expr) :> i#)
    i#)))

(defmacro debug-2 [expr]
  `(do (println (list ~@expr))
    ~expr))

(defmacro debug-3 [expr]
  `(let [val# ~expr]
    (do (println '~expr :> val#)
      val#)))

;;;;
;;;; Multiple execution
;;;;

(defmacro and-1 [expr1 expr2]
  ...)


(defmacro and-1 [expr1 expr2]
  `(let [val1# ~expr1]
    (if val1#
      ~expr2
      val1#)))

;;;;
;;;; Using functions in macros
;;;;

(defn stringify-static [parts]
  (map
    (fn [part]
      (if
        (or (keyword? part)
            (string? part)
            (number? part))
        (str part)
        part))
    parts))

(defn constant? (some-fun keyword? string? number?))

(defn stringify-static [parts]
  (map
    (fn [part]
      (if (constant? part)
        (str part)
        part))
    parts))

; TODO Merge adjacent strings

(defmacro pre-str [& parts]
  `(str ~@(stringify-static parts)))

;;;;
;;;; Recursive macros
;;;;

(defmacro and-* 
  ([] ...)
  ([expr] ...)
  ([expr & more] ...))

