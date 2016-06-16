(ns ^{:doc "Specs for clojure.core macros."}
    spec.clojure.macros
  (:require [clojure.spec :as s]))

;;; binding vector specs

(s/def ::destructuring-target-vec
  (s/and vector?
         (s/cat :pos-bindings (s/* ::destructuring-target)
                :rest-binding (s/? (s/cat :amp '#{&}
                                          :rest ::destructuring-target))
                :as (s/? (s/cat :as #{:as} :sym symbol?)))))

(s/def ::destructuring-target-map-kv
  (s/or :keys (s/tuple #{:keys} (s/coll-of symbol? []))
        :syms (s/tuple #{:syms} (s/coll-of symbol? []))
        :strs (s/tuple #{:strs} (s/coll-of symbol? []))
        :as (s/tuple #{:as} symbol?)
        :or (s/tuple #{:or} (s/map-of symbol? ::s/any))
        :kv (s/tuple ::destructuring-target ::s/any)))

(s/def ::destructuring-target-map
  (s/and map? (s/coll-of ::destructuring-target-map-kv {})))

(s/def ::destructuring-target
  (s/or :symbol (s/and symbol? #(not= '& %)) ;TODO ugly hack (and not correct)
        :vec ::destructuring-target-vec
        :map ::destructuring-target-map))

(s/def ::let-binding
  (s/cat :target ::destructuring-target
         :val ::s/any))

(s/def ::let-bindings
  (s/and vector? (s/* ::let-binding)))

(s/def ::simple-let-bindings ;; let*, loop*, binding
  (s/and vector? (s/* (s/cat :target symbol? :val ::s/any))))

(s/def ::for-comprehension-op
  (s/alt :let (s/cat :let #{:let} :bindings ::let-bindings)
         :when (s/cat :when #{:when} :condition ::s/any)
         :while (s/cat :while #{:while} :condition ::s/any)))

(s/def ::for-comprehension-pair
  (s/alt :binding ::let-binding
         :op ::for-comprehension-op))

(s/def ::for-comprehension-bindings
  (s/and vector? (s/* ::for-comprehension-pair)))

(comment
  (->> 'clojure.core the-ns ns-publics vals
       (filter (comp :macro meta))
       (map (comp :name meta))
       sort)
  ;; =>
  (
   ;;DONE
   ;; amap areduce as-> binding declare doseq dotimes for if-let if-some let
   ;; loop when-first when-let when-some with-local-vars with-open
   ;; with-precision with-redefs

   ;;TODO
   ;; bound-fn
   ;; case ; not sure it's possible to type (on keyboard) something that's not
   ;;      ; technically a compile-time literal
   ;; definline definterface defmacro defmethod defmulti defn defn- defonce
   ;; defprotocol defrecord defstruct deftype extend-protocol extend-type fn
   ;; gen-class gen-interface import letfn memfn ns proxy proxy-super
   ;; refer-clojure reify sync

   ;;TODO input too general to be specified?
   ;; -> ->> .. and assert comment cond cond-> cond->> condp delay dosync doto
   ;; future if-not io! lazy-cat lazy-seq locking or pvalues some-> some->> time
   ;; vswap! when when-not while with-bindings with-in-str with-loading-context
   ;; with-out-str
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clojure.core macro specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef clojure.core/amap
        :args (s/cat :array ::s/any ;; TODO not quite
                     :idx symbol?
                     :ret symbol?
                     :expr ::s/any)
        :ret ::s/any)

(s/fdef clojure.core/areduce
        :args (s/cat :array ::s/any ;; TODO not quite
                     :idx symbol?
                     :ret symbol?
                     :init ::s/any
                     :expr ::s/any)
        :ret ::s/any)

(s/fdef clojure.core/as->
        :args (s/cat :expr ::s/any
                     :name ::destructuring-target
                     :forms (s/* ::s/any))
        :ret ::s/any)

(s/fdef clojure.core/binding
        :args (s/cat :bindings ::simple-let-bindings
                     :body (s/* ::s/any))
        :ret ::s/any)

(s/fdef clojure.core/declare
        :args (s/cat :names (s/* symbol?))
        :ret ::s/any)

(s/fdef clojure.core/let
        :args (s/cat :bindings ::let-bindings
                     :body (s/* ::s/any))
        :ret ::s/any)

(s/fdef clojure.core/loop
        :args (s/cat :bindings ::let-bindings
                     :body (s/* ::s/any))
        :ret ::s/any)

(s/fdef clojure.core/for
        :args (s/cat :bindings ::for-comprehension-bindings
                     :body-expr ::s/any)
        :ret ::s/any)

(s/fdef clojure.core/doseq
        :args (s/cat :bindings ::for-comprehension-bindings
                     :body (s/* ::s/any))
        :ret ::s/any)

(s/fdef clojure.core/dotimes
        :args (s/cat :bindings (s/and vector? (s/cat :name symbol? :n ::s/any))
                     :body (s/* ::s/any))
        :ret ::s/any)

(s/fdef clojure.core/if-let
        :args (s/cat :bindings (s/and vector? ::let-binding)
                     :then ::s/any
                     :else (s/? ::s/any))
        :ret ::s/any)

(s/fdef clojure.core/if-some
        :args (s/cat :bindings (s/and vector? ::let-binding)
                     :then ::s/any
                     :else (s/? ::s/any))
        :ret ::s/any)

(s/fdef clojure.core/when-first
        :args (s/cat :bindings (s/and vector? ::let-binding)
                     :body (s/* ::s/any))
        :ret ::s/any)

(s/fdef clojure.core/when-let
        :args (s/cat :bindings (s/and vector? ::let-binding)
                     :body (s/* ::s/any))
        :ret ::s/any)

(s/fdef clojure.core/when-some
        :args (s/cat :bindings (s/and vector? ::let-binding)
                     :body (s/* ::s/any))
        :ret ::s/any)

(s/fdef clojure.core/with-local-vars
        :args (s/cat :bindings ::simple-let-bindings
                     :body (s/* ::s/any))
        :ret ::s/any)

(s/fdef clojure.core/with-open
        :args (s/cat :bindings ::simple-let-bindings
                     :body (s/* ::s/any))
        :ret ::s/any)

;;TODO ughly.
(s/fdef clojure.core/with-precision
        :args (s/alt
               :with-rounding-mode
               (s/cat :precision ::s/any
                      :rounding-kw #{:rounding}
                      :rounding-mode '#{CEILING, FLOOR, HALF_UP,
                                        HALF_DOWN,HALF_EVEN, UP,
                                        DOWN, UNNECESSARY}
                      :body (s/* ::s/any))
               :without-rounding-mode
               (s/cat :precision ::s/any
                      :body (s/* ::s/any)))
        :ret ::s/any)

(s/fdef clojure.core/with-redefs
        :args (s/cat :bindings ::simple-let-bindings
                     :body (s/* ::s/any))
        :ret ::s/any)
