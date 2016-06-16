(ns ^{:doc "Specs for clojure.core fns."}
    spec.clojure.fns
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]))

;;; useful predicates

(defn ex-info? [x] (instance? clojure.lang.ExceptionInfo x))
(defn namespace? [x] (instance? clojure.lang.Namespace x))
(defn queue? [x] (instance? clojure.lang.PersistentQueue x))
(defn multi-fn? [x] (instance? clojure.lang.MultiFn x))
(defn atom? [x] (instance? clojure.lang.IAtom x))
(let [atypes (mapv #(type (% 0))
                   [boolean-array byte-array char-array double-array float-array
                    int-array long-array object-array short-array])]
  (defn array? [x] (some #(instance? % x) atypes)))

;;; gens

(defn gen-short []
  (gen/fmap short (gen/choose Short/MIN_VALUE Short/MAX_VALUE)))

(s/def ::array?
  (s/with-gen array?
    (fn [] (gen/fmap into-array
                     (gen/one-of
                      [;;gen/bytes ;;TODO
                        (gen/vector (gen-short))
                        (gen/vector (gen/int))
                        (gen/vector (gen/boolean))
                        (gen/vector (gen/double))
                        (gen/vector (gen/keyword))]))))) ; stand-in for Object

;;TODO 'any' gen seems to mean 'any collection'

(s/def ::queue?
  (s/with-gen queue?
    (fn [] (gen/fmap #(into clojure.lang.PersistentQueue/EMPTY %)
                     (gen/vector (s/gen ::s/any))))))

(s/def ::atom?
  (s/with-gen atom?
    (fn [] (gen/fmap atom (gen/any-printable)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clojure.core fn specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef clojure.core/identical?
  :args (s/cat :x ::s/any :y ::s/any)
  :ret boolean?)

(s/fdef clojure.core/number?
  :args (s/cat :x ::s/any)
  :ret boolean?)

(s/fdef clojure.core/not
  :args (s/cat :x ::s/any)
  :ret boolean?)

(s/fdef clojure.core/string?
  :args (s/cat :x ::s/any)
  :ret boolean?)

(s/fdef clojure.core/type
  :ares (s/cat :x ::s/any)
  :ret ::s/any)

(s/fdef clojure.core/aclone
  :args (s/cat :array ::array?)
  :ret ::array?)

(s/fdef clojure.core/aget
  :args (s/cat :array ::array?
               :idx integer?
               :idxs (s/* integer?))
  :ret ::s/any
  :fn #(let [{{:keys [array idxs]} :args} %
             n (inc (count idxs))]
         (every? array? (take n (iterate first array)))))
;;TODO :fn check should actually go in the :args spec?

(s/fdef clojure.core/aset
   :args (s/cat
          :array ::array?
          :idx integer?
          :idxs (s/* integer?)
          :val ::s/any)
   :ret ::s/any
   :fn #(let [{{:keys [array idxs]} :args} %
              n (inc (count idxs))]
          (every? array? (take n (iterate first array)))))
;;TODO :fn check should actually go in the :args spec?

(s/fdef clojure.core/alength
  :args (s/cat :array ::array?)
  :ret integer?)

(s/fdef clojure.core/symbol?
  :args (s/cat :x ::s/any)
  :ret boolean?)

(s/fdef clojure.core/symbol
  :args (s/alt
         :name (s/or
                :symbol? symbol?
                :string? string?)
         :ns-name (s/cat
                   :ns (s/nilable string?)
                   :name string?))
  :ret symbol?)

(s/fdef clojure.core/seq
   :args (s/cat :coll seqable?)
   :ret (s/nilable seq?)
   :fn (s/or :empty #(and (empty? (-> % :args :coll))
                          (nil? (:ret %)))
             :not-empty #(and (not-empty (-> % :args :coll))
                              (not-empty (:ret %)))))

(s/fdef clojure.core/first
  :args (s/cat :coll seqable?)
   :ret (s/nilable ::s/any)
   :fn (s/or :empty #(and (empty? (-> % :args :coll))
                          (nil? (:ret %)))
             :not-empty ::s/any))

(s/fdef clojure.core/rest
  :args (s/cat :coll seqable?)
  :ret seq?)

;;TODO maybe not the best idea to apply count to arbitrary colls
(s/fdef clojure.core/next
  :args (s/cat :coll seqable?)
   :ret (s/nilable seq?)
   :fn (s/or :count-<2 #(and (< (count (-> % :args :coll)) 2)
                             (nil? (:ret %)))
             :count->=2 #(not-empty (:ret %))))

(s/fdef clojure.core/=
  :args (s/cat :args (s/+ ::s/any))
  :ret boolean?)

;;TODO maybe not the best idea to apply count to arbitrary colls
(s/fdef clojure.core/second
  :args (s/cat :coll seqable?)
  :ret (s/nilable ::s/any)
  :fn (s/or :count-<2 #(and (< (count (-> % :args :coll)) 2)
                            (nil? (:ret %)))
            :count->=2 ::s/any))

(s/fdef clojure.core/ffirst
  :args (s/cat :x seqable?)
  :ret (s/nilable ::s/any))

(s/fdef clojure.core/nnext
  :args (s/cat :x seqable?)
  :ret seq?)

(s/fdef clojure.core/last
  :args (s/cat :coll seqable?)
   :ret ::s/any
   :fn (s/or :empty #(and (empty? (-> % :args :coll))
                          (nil? (:ret %)))
             :not-empty ::s/any))

;;TODO nowhere near the whole story, this fn is *very* polymorphic
(s/fdef clojure.core/conj
  :args (s/cat
         :coll (s/or :nil? nil? :coll? coll? :seq? seq?)
         :x ::s/any
         :xs (s/* ::s/any))
  :ret (s/or :coll? coll? :seq? seq?))

(s/fdef clojure.core/get
   :args (s/cat
          :map (s/or
                :nil? nil?
                :set? set?
                :associative? associative?)
          :key ::s/any
          :not-found (s/? ::s/any))
   :ret ::s/any)

(s/fdef clojure.core/assoc
   :args (s/cat
          :coll (s/or :map (s/map-of ::s/any ::s/any)
                      :vec vector?)
          :key ::s/any
          :val ::s/any)
   :ret associative?
   :fn (s/or :vector #(let [v (-> % :args :coll)
                            i (-> % :args :key)]
                        (and (vector? v)
                             (integer? i)
                             (< i (count v))
                             (vector? (:ret %))))
             :map #(and (map? (-> % :args :coll))
                        (map? (:ret %)))))

(s/fdef clojure.core/dissoc
  :args (s/cat :map map?
               :k ::s/any
               :ks (s/* ::s/any))
  :ret map?)

(s/fdef clojure.core/meta
  :args (s/cat :obj ::s/any)
  :ret (s/nilable (s/map-of ::s/any ::s/any)))

(s/fdef clojure.core/peek
  :args (s/cat
         :coll (s/or
                :list list?
                :vec vector?
                :queue ::queue?))
  :ret ::s/any
  :fn #(if (empty? (-> % :args :coll second))
         (nil? (:ret %))
         true))

(s/fdef clojure.core/disj
  :args (s/cat
         :set set?
         :key ::s/any
         :ks (s/* ::s/any))
  :ret set?)

(s/fdef clojure.core/hash
  :args (s/cat :x ::s/any)
  :ret integer?)

(s/fdef clojure.core/empty?
   :args (s/cat :coll seqable?)
   :ret boolean?)

(s/fdef clojure.core/coll?
  :args (s/cat :x ::s/any)
  :ret boolean?)

(s/fdef clojure.core/map?
  :args (s/cat :x ::s/any)
  :ret boolean?)

(s/fdef clojure.core/vector?
  :args (s/cat :x ::s/any)
  :ret boolean?)

(s/fdef clojure.core/chunked-seq?
  :args (s/cat :s ::s/any)
  :ret ::s/any)

(s/fdef clojure.core/false?
  :args (s/cat :x ::s/any)
  :ret boolean?)

(s/fdef clojure.core/true?
  :args (s/cat :x ::s/any)
  :ret boolean?)

(s/fdef clojure.core/seq?
  :args (s/cat :x ::s/any)
  :ret boolean?)

(s/fdef clojure.core/boolean
  :args (s/cat :x ::s/any)
  :ret boolean?)

(s/fdef clojure.core/integer?
  :args (s/cat :x ::s/any)
  :ret boolean?)

(s/fdef clojure.core/contains?
  :args (s/cat
         :coll seqable?
         :key ::s/any)
  :ret boolean?)

(s/fdef clojure.core/find
  :args (s/cat
         :map (s/nilable associative?)
         :key ::s/any)
  :ret (s/nilable (s/and (s/tuple ::s/any ::s/any) map-entry?)))

(s/fdef clojure.core/distinct?
  :args (s/cat :args (s/+ ::s/any))
  :ret boolean?)

(s/fdef clojure.core/compare
  :args (s/cat :x ::s/any :y ::s/any)
  :ret number?)

(s/fdef clojure.core/sort
  :args (s/cat
         :comp (s/? (s/fspec
                     :args (s/cat :x ::s/any :y ::s/any)
                     :ret integer?))
         :coll seqable?)
   :ret seq?
   :fn #(= (count (-> % :args :coll))
           (count (:ret %))))
;;TODO core.typed seems to think sort could return nil, but I don't see how.

(s/fdef clojure.core/shuffle
   :args (s/cat :coll seqable?) ;TODO actually, iterable
   :ret vector?
   :fn #(= (count (-> % :args :coll))
           (count (:ret %))))

(s/fdef clojure.core/reduce
   :args (s/cat
          :f (s/fspec
              :args (s/cat :acc ::s/any :x ::s/any)
              :ret (s/or :reduced? reduced? :any ::s/any))
          :val (s/? ::s/any)
          :coll seqable?)
   :ret ::s/any)
;;TODO actual constraints are complicated, involve f's arity and number of items
;; in coll

(s/fdef clojure.core/reduce-kv
  :args (s/cat
         :f (s/fspec
             :args (s/cat
                    :acc ::s/any
                    :k ::s/any
                    :v ::s/any)
             :ret (s/or :reduced? reduced? :any ::s/any))
         :init ::s/any
         :coll (s/nilable associative?))
  :ret ::s/any)

(s/fdef clojure.core/<
  :args (s/cat :args (s/+ number?))
  :ret boolean?)

(s/fdef clojure.core/<=
  :args (s/cat :args (s/+ number?))
  :ret boolean?)

(s/fdef clojure.core/>
  :args (s/cat :args (s/+ number?))
  :ret boolean?)

(s/fdef clojure.core/>=
  :args (s/cat :args (s/+ number?))
  :ret boolean?)

(s/fdef clojure.core/==
  :args (s/cat :args (s/+ number?))
  :ret boolean?)

(s/fdef clojure.core/max
  :args (s/cat :args (s/+ number?))
  :ret number?)

(s/fdef clojure.core/min
  :args (s/cat :args (s/+ number?))
  :ret number?)

(s/fdef clojure.core/int
  :args (s/cat :x number?)
  :ret integer?)

(s/fdef clojure.core/booleans
  :args (s/cat :xs ::s/any)
  :ret ::array?)

(s/fdef clojure.core/ints
  :args (s/cat :xs ::s/any)
  :ret ::array?)

(s/fdef clojure.core/mod
  :args (s/cat :num number? :div number?)
  :ret number?
  :fn #(let [{ret :ret {:keys [num div]} :args} %]
         (and (if (and (integer? num) (integer? div))
                (integer? ret)
                true)
              (if (pos? div)
                (<= 0 ret div)
                (<= div ret 0)))))

(s/fdef clojure.core/rand
   :args (s/cat :n (s/? number?))
   :ret number?)

(s/fdef clojure.core/rand-int
  :args (s/cat :n integer?)
  :ret integer?)

(s/fdef clojure.core/random-sample
  :args (s/cat
         :prob (s/double-in :min 0.0 :max 1.0)
         :coll (s/? seqable?))
  :ret (s/or
        :transducer fn?
        :seq seq?)
  :fn #(let [ret-type (-> % :ret first)]
         (if (-> % :args (contains? :coll))
           (= ret-type :seq)
           (= ret-type :transducer))))
;;TODO the actual spec for current allowed :prob values is just number?
;; Should the spec cover everything currently allowed, or restrict things
;; to what users *should* be passing in?

(s/fdef clojure.core/bit-xor
  :args (s/cat
         :x integer?
         :y integer?
         :more (s/* integer?))
  :ret integer?)

(s/fdef clojure.core/bit-or
  :args (s/cat
         :x integer?
         :y integer?
         :more (s/* integer?))
  :ret integer?)

(s/fdef clojure.core/bit-and-not
  :args (s/cat
         :x integer?
         :y integer?
         :more (s/* integer?))
  :ret integer?)

(s/fdef clojure.core/bit-clear
  :args (s/cat :x integer? :n integer?)
  :ret integer?)

(s/fdef clojure.core/bit-flip
  :args (s/cat :x integer? :n integer?)
  :ret integer?)

(s/fdef clojure.core/bit-not
  :args (s/cat :x integer?)
  :ret integer?)

(s/fdef clojure.core/bit-set
  :args (s/cat :x integer? :n integer?)
  :ret integer?)

(s/fdef clojure.core/bit-test
  :args (s/cat :x integer? :n integer?)
  :ret integer?)

(s/fdef clojure.core/bit-shift-left
  :args (s/cat :x integer? :n integer?)
  :ret integer?)

(s/fdef clojure.core/bit-shift-right
  :args (s/cat :x integer? :n integer?)
  :ret integer?)

(s/fdef clojure.core/pos?
  :args (s/cat :x number?)
  :ret boolean?)

(s/fdef clojure.core/neg?
  :args (s/cat :x number?)
  :ret boolean?)

(s/fdef clojure.core/nthnext
   :args (s/cat :coll seqable? :n integer?)
   :ret (s/nilable seq?)
   :fn (let [len (count (-> % :args :coll))]
         (if (<= len (-> % :args :n))
           (nil? (:ret %))
           (not-empty (:ret %)))))

(s/fdef clojure.core/str
  :args (s/cat :args (s/* ::s/any))
  :ret string?)

(s/fdef clojure.core/subs
   :args (s/cat
          :s string?
          :start integer?
          :end (s/? integer?))
   :ret string?)

(s/fdef clojure.core/hash-combine
  :args (s/cat :x integer? :y ::s/any)
  :ret integer?)

(s/fdef clojure.core/reverse
  :args (s/cat :coll seqable?)
  :ret seq?)

(s/fdef clojure.core/list
  :args (s/cat :items (s/* ::s/any))
  :ret list?)

(s/fdef clojure.core/cons
  :args (s/cat
         :x ::s/any
         :seq seqable?)
  :ret seq?)

(s/fdef clojure.core/list?
  :args (s/cat :x ::s/any)
  :ret boolean?)

(s/fdef clojure.core/keyword?
  :args (s/cat :x ::s/any)
  :ret boolean?)

(s/fdef clojure.core/namespace
  :args (s/cat
         :x (s/or
             :symbol? symbol?
             :string? string?
             :keyword? keyword?))
  :ret (s/nilable string?))

(s/fdef clojure.core/keyword
   :args (s/alt
           :no-ns (s/cat
                    :name (s/or
                           :keyword? keyword?
                           :symbol? symbol?
                           :string? string?))
           :with-ns (s/cat
                      :ns string?
                      :name string?)
   :ret keyword?))

(s/fdef clojure.core/int-array
   :args (s/alt
           :size-or-seq
           (s/cat
             :size-or-seq (s/or
                           :number? number?
                           :seqable? seqable?))
           :size-and-init
           (s/cat
             :size number?
             :init-val-or-seq (s/or
                                :number? number?
                                :seqable? seqable?))
   :ret ::array?))

(s/fdef clojure.core/concat
  :args (s/cat :colls (s/* seqable?))
  :ret seq?)

(s/fdef clojure.core/list*
  :args (s/cat
         :items (s/* ::s/any)
         :list seqable?)
  :ret (s/nilable seq?))

;;TODO merely trying to instrument this borks the repl.
(s/fdef clojure.core/apply
   :args (s/cat
          :f (s/fspec
              :args (s/* ::s/any)
              :ret ::s/any)
          :items (s/* ::s/any)
          :list seqable?)
   :ret ::s/any)
;; again, complicated constraints involving fn arities

(s/fdef clojure.core/not=
  :args (s/cat :args (s/+ ::s/any))
  :ret boolean?)

(s/fdef clojure.core/every?
    :args (s/cat
           :pred (s/fspec
                  :args (s/cat :x ::s/any)
                  :ret ::s/any)
           :coll seqable?)
    :ret boolean?)

(s/fdef clojure.core/some
  :args (s/cat
         :pred (s/fspec
                :args (s/cat :x ::s/any)
                :ret ::s/any)
         :coll seqable?)
  :ret (s/nilable ::s/any))

(s/fdef clojure.core/even?
  :args (s/cat :n integer?)
  :ret boolean?)

(s/fdef clojure.core/odd?
  :args (s/cat :n integer?)
  :ret boolean?)

(s/fdef clojure.core/identity
  :args (s/cat :x ::s/any)
  :ret ::s/any)

(s/fdef clojure.core/complement
  :args (s/cat
         :f (s/fspec
             :args (s/cat :x ::s/any)
             :ret ::s/any))
  :ret (s/fspec
        :args (s/cat :f ::s/any)
        :ret boolean?))

(s/fdef clojure.core/constantly
  :args (s/cat :x ::s/any)
  :ret (s/fspec
        :args (s/cat :args (s/* ::s/any))
        :ret ::s/any)
  :fn #(= (-> % :args :x)
          ((:ret %))
          ((:ret %) :any)
          ((:ret %) :any :thing)))

;;TODO arbitrary-arity comp is hard enough to deal with in a formal type system,
;; I don't have any idea how to deal with it here.  And the same is true for a
;; lot of the higher-order fns.
(s/fdef clojure.core/comp
  :args (s/cat :fns (s/* ifn?))
  :ret (s/fspec
        :args (s/cat :args (s/* ::s/any))
        :ret ::s/any))

(s/fdef clojure.core/partial
   :args (s/cat
          :f (s/fspec
              :args (s/* ::s/any)
              :ret ::s/any)
          :args (s/* ::s/any))
   :ret (s/fspec
         :args (s/* ::s/any)
         :ret ::s/any))
;; again, complicated constraints involving fn arities (and the whole fspec of
;; f, if it exists)

(s/fdef clojure.core/fnil
   :args (s/cat
          :f (s/fspec :args (s/+ ::s/any) :ret ::s/any)
          :x ::s/any
          :y (s/? ::s/any)
          :z (s/? ::s/any)::s/any)
   :ret (s/fspec :args (s/+ ::s/any) :ret ::s/any))
;; again, complicated constraints involving fn arities (and the whole fspec of
;; f, if it exists)

(s/fdef clojure.core/map-indexed
  :args (s/cat
         :f (s/fspec
             :args (s/cat
                    :i integer?
                    :x ::s/any)
             :ret ::s/any)
         :coll (s/? seqable?))
  :ret (s/or
        :transducer fn?
        :seq seq?)
  :fn #(let [ret-type (-> % :ret first)]
         (if (-> % :args (contains? :coll))
           (= ret-type :seq)
           (= ret-type :transducer))))

(s/fdef clojure.core/every-pred
  :args (s/cat
         :p (s/fspec
             :args (s/cat :x ::s/any)
             :ret boolean?)
         :ps (s/*
               (s/fspec
                :args (s/cat :x ::s/any)
                :ret boolean?))) ;TODO s/+ instead?
  :ret (s/fspec
        :args (s/* ::s/any)
        :ret boolean?))

(s/fdef clojure.core/map
  :args (s/cat :f ifn? :colls (s/* seqable?))
   :ret (s/or
         :transducer fn?
         :seq seq?)
   :fn #(let [ret-type (-> % :ret first)]
          (if (-> % :args :colls not-empty)
            (= ret-type :seq)
            (= ret-type :transducer))))

(s/fdef clojure.core/mapcat
  :args (s/alt
          :transducer (s/cat :f ifn?)
          :seq (s/cat
                :f (s/fspec
                    :args (s/* ::s/any)
                    :ret seqable?)
                :colls (s/+ seqable?)))
  :ret (s/or :transducer fn?
             :seq seq?))

(s/fdef clojure.core/pmap
    :args (s/cat :f ifn? :colls (s/+ seqable?))
    :ret seq?)

(s/fdef clojure.core/take
  :args (s/cat :n integer? :coll seqable?)
  :ret (s/or
        :transducer fn?
        :seq seq?)
  :fn #(let [ret-type (-> % :ret first)]
         (if (-> % :args (contains? :coll))
           (= ret-type :seq)
           (= ret-type :transducer))))

(s/fdef clojure.core/drop
  :args (s/cat :n integer? :coll (s/? seqable?))
  :ret (s/or
        :transducer fn?
        :seq seq?)
  :fn #(let [ret-type (-> % :ret first)]
         (if (-> % :args (contains? :coll))
           (= ret-type :seq)
           (= ret-type :transducer))))

(s/fdef clojure.core/drop-last
  :args (s/cat :n integer? :s seqable?)
  :ret seq?)

(s/fdef clojure.core/take-last
  :args (s/cat :n integer? :coll seqable?)
  :ret seq?)

(s/fdef clojure.core/drop-while
  :args (s/cat
         :pred (s/fspec
                :args (s/cat :x ::s/any)
                :ret ::s/any)
         :coll (s/? seqable?))
  :ret (s/or
        :transducer fn?
        :seq seq?)
  :fn #(let [ret-type (-> % :ret first)]
         (if (-> % :args (contains? :coll))
           (= ret-type :seq)
           (= ret-type :transducer))))

(s/fdef clojure.core/cycle
  :args (s/cat :coll seqable?)
  :ret seq?)

(s/fdef clojure.core/split-at
  :args (s/cat :n integer? :coll seqable?)
  :ret (s/tuple seq? seq?))

(s/fdef clojure.core/repeat
   :args (s/alt
           :sized (s/cat :n integer? :x ::s/any)
           :infinite (s/cat :x ::s/any))
   :ret seq?)

(s/fdef clojure.core/repeatedly
   :args (s/alt
           :sized (s/cat
                    :n integer?
                    :f (s/fspec :args (s/cat) :ret ::s/any))
           :infinite (s/cat
                       :f (s/fspec :args (s/cat) :ret ::s/any)))
   :ret seq?)

(s/fdef clojure.core/iterate
  :args (s/cat
         :f (s/fspec
             :args (s/cat :x ::s/any)
             :ret ::s/any)
         :x ::s/any)
  :ret seq?)

(s/fdef clojure.core/interleave
  :args (s/cat
         :c1 seqable?
         :c2 seqable?
         :colls (s/* seqable?))
  :ret seq?)

(s/fdef clojure.core/interpose
  :args (s/cat
         :sep ::s/any
         :coll (s/? seqable?))
  :ret (s/or
        :transducer fn?
        :seq seq?)
  :fn #(let [ret-type (-> % :ret first)]
         (if (-> % :args (contains? :coll))
           (= ret-type :seq)
           (= ret-type :transducer))))

(s/fdef clojure.core/filter
  :args (s/alt
         :transducer (s/cat
                      :pred (s/fspec :args (s/cat :x ::s/any) :ret ::s/any))
         :seq (s/cat
               :pred (s/fspec :args (s/cat :x ::s/any) :ret ::s/any)
               :coll seqable?))
    :ret (s/or :transducer fn?
               :seq seq?))

(s/fdef clojure.core/remove
  :args (s/alt
         :transducer (s/cat
                      :pred (s/fspec :args (s/cat :x ::s/any) :ret ::s/any))
         :seq (s/cat
               :pred (s/fspec :args (s/cat :x ::s/any) :ret ::s/any)
               :coll seqable?))
  :ret (s/or :transducer fn?
             :seq seq?))

(s/fdef clojure.core/flatten
  :args (s/cat :x ::s/any)
  :ret ::s/any)
;;TODO can this be stricter?

(s/fdef clojure.core/into
  :args (s/cat
         :to coll?
         :xform (s/? fn?)
         :from seqable?)
  :ret coll?
  :fn #(let [to (-> % :args :to)
             ret (:ret %)]
         (or (and (vector? to) (vector? ret))
             (and (map? to) (map? ret))
             (and (set? to) (set? ret))
             (and (seq? to) (seq? ret)))))
;;TODO constraint not quite right

(s/fdef clojure.core/mapv
   :args (s/cat :f ifn? :colls (s/+ seqable?))
   :ret vector?
   ;;TODO maybe not the best idea to apply count to arbitrary colls
   ;; :fn #(= (count (:ret %))
   ;;         (->> (-> % :args :colls)
   ;;              (map count)
   ;;              (apply min)))
)


(s/fdef clojure.core/filterv
    :args (s/cat
           :pred (s/fspec
                  :args (s/cat :x ::s/any)
                  :ret ::s/any)
           :coll seqable?)
    :ret vector?
    :fn #(<= 0
             (count (:ret %))
             (count (-> % :args :coll))))

(s/fdef clojure.core/get-in
    :args (s/cat
           :m ::s/any
           :ks seqable?
           :not-found (s/? ::s/any))
    :ret ::s/any)

(s/fdef clojure.core/assoc-in
  :args (s/cat
         :m (s/nilable associative?)
         :ks seqable?
         :v ::s/any)
  :ret ::s/any)

(s/fdef clojure.core/vec
  :args (s/cat :coll seqable?)
  :ret vector?)

(s/fdef clojure.core/vector
  :args (s/cat :args (s/* ::s/any))
  :ret vector?
  :fn #(= (count (:ret %))
          (count (-> % :args :args))))

(s/fdef clojure.core/subvec
    :args (s/cat
           :v vector?
           :start integer?
           :end (s/? integer?))
    :ret vector?)

(s/fdef clojure.core/keys
  :args (s/cat :map (s/map-of ::s/any ::s/any))
  :ret seq?)

(s/fdef clojure.core/key
  :args (s/cat :e map-entry?)
  :ret ::s/any)

(s/fdef clojure.core/vals
  :args (s/cat :map (s/map-of ::s/any ::s/any))
  :ret seq?)

(s/fdef clojure.core/val
  :args (s/cat :e map-entry?)
  :ret ::s/any)

(s/fdef clojure.core/merge
    :args (s/cat :maps (s/* (s/nilable (s/map-of ::s/any ::s/any))))
    :ret (s/nilable (s/map-of ::s/any ::s/any)))

(s/fdef clojure.core/merge-with
    :args (s/cat
           :f (s/fspec
               :args (s/cat :x ::s/any :y ::s/any)
               :ret ::s/any)
           :maps (s/* (s/nilable (s/map-of ::s/any ::s/any))))
    :ret (s/nilable (s/map-of ::s/any ::s/any)))

(s/fdef clojure.core/select-keys
  :args (s/cat
         :map (s/map-of ::s/any ::s/any)
         :keyseq seqable?)
  :ret (s/map-of ::s/any ::s/any))

(s/fdef clojure.core/distinct
  :args (s/cat :coll seqable?)
  :ret seq?)

(s/fdef clojure.core/butlast
  :args (s/cat :coll seqable?)
  :ret seq?)

(s/fdef clojure.core/zipmap
  :args (s/cat
         :keys seqable?
         :vals seqable?)
  :ret (s/map-of ::s/any ::s/any))

(s/fdef clojure.core/max-key
  :args (s/cat
         :k (s/fspec
             :args (s/cat :x ::s/any)
             :ret number?)
         :x ::s/any
         :y ::s/any
         :more (s/* ::s/any))
  :ret ::s/any)

(s/fdef clojure.core/min-key
  :args (s/cat
         :k (s/fspec
             :args (s/cat :x ::s/any)
             :ret number?)
         :x ::s/any
         :y ::s/any
         :more (s/* ::s/any))
  :ret ::s/any)

(s/fdef clojure.core/partition-all
  :args (s/alt
         :transducer (s/cat :n integer?)
         :seq (s/cat
               :n integer?
               :step (s/? integer?)
               :coll seqable?))
  :ret (s/or :transducer fn?
             :seq seq?))

;;TODO partition-by

(s/fdef clojure.core/take-while
  :args (s/cat
         :pred (s/fspec
                :args (s/cat :x ::s/any)
                :ret ::s/any)
         :coll seqable?)
  :ret (s/or
        :transducer fn?
        :seq seq?)
  :fn #(let [ret-type (-> % :ret first)]
         (if (-> % :args (contains? :coll))
           (= ret-type :seq)
           (= ret-type :transducer))))

(s/fdef clojure.core/range
  :args (s/alt
         :infinite (s/cat)
         :end (s/cat :end number?)
         :start-end (s/cat :start number? :end number?)
         :start-end-step (s/cat
                          :start number?
                          :end number?
                          :step number?)
   :ret seq?))

(s/fdef clojure.core/take-nth
  :args (s/cat
         :n integer?
         :coll (s/? seqable?))
  :ret (s/or
        :transducer fn?
        :seq seq?)
  :fn #(let [ret-type (-> % :ret first)]
         (if (-> % :args (contains? :coll))
           (= ret-type :seq)
           (= ret-type :transducer))))

(s/fdef clojure.core/split-with
  :args (s/cat
         :pred (s/fspec
                :args (s/cat :x ::s/any)
                :ret ::s/any)
         :coll seqable?)
  :ret (s/tuple seq? seq?))

(s/fdef clojure.core/dorun
  :args (s/cat
         :n (s/? integer?)
         :coll seqable?)
  :ret nil?)

(s/fdef clojure.core/doall
  :args (s/cat :n (s/? integer?) :coll seqable?)
  :ret seqable?)

(s/fdef clojure.core/newline
  :args (s/cat) :ret nil?)

(s/fdef clojure.core/prn-str
  :args (s/cat :xs (s/* ::s/any))
  :ret string?)

(s/fdef clojure.core/pr-str
  :args (s/cat :xs (s/* ::s/any))
  :ret string?)

(s/fdef clojure.core/pr
  :args (s/cat :xs (s/* ::s/any))
  :ret nil?)

(s/fdef clojure.core/print
  :args (s/cat :more (s/* ::s/any))
  :ret nil?)

(s/fdef clojure.core/println
  :args (s/cat :more (s/* ::s/any))
  :ret nil?)

(s/fdef clojure.core/print-str
  :args (s/cat :xs (s/* ::s/any))
  :ret string?)

(s/fdef clojure.core/println-str
  :args (s/cat :xs (s/* ::s/any))
  :ret string?)

(s/fdef clojure.core/prn
  :args (s/cat :more (s/* ::s/any))
  :ret nil?)

(s/fdef clojure.core/atom
  :args (s/cat :x ::s/any)
  :ret atom?)

(s/fdef clojure.core/reset!
  :args (s/cat :atom atom? :newval ::s/any)
  :ret ::s/any)

(s/fdef clojure.core/swap!
  :args (s/cat :atom ::atom?
               :f ifn?
               :args (s/* ::s/any))
  :ret ::s/any)

(s/fdef clojure.core/compare-and-set!
  :args (s/cat
         :atom atom?
         :oldval ::s/any
         :newval ::s/any)
  :ret boolean?)

(s/fdef clojure.core/gensym
  :args (s/cat
         :prefix-string (s/? (s/or
                              :symbol? symbol?
                              :string? string?)))
  :ret symbol?)

(s/fdef clojure.core/delay?
  :args (s/cat :x ::s/any)
  :ret boolean?)

(s/fdef clojure.core/force
  :args (s/cat :x ::s/any)
  :ret ::s/any)

(s/fdef clojure.core/memoize
  :args (s/cat
         :f (s/fspec :args (s/* ::s/any) :ret ::s/any))
  :ret (s/fspec :args (s/* ::s/any) :ret ::s/any))

(s/fdef clojure.core/trampoline
  :args (s/cat
         :f (s/fspec :args (s/* ::s/any) :ret ::s/any)
         :args (s/* ::s/any))
  :ret #(not (fn? %)))

(s/fdef clojure.core/isa?
  :args (s/cat :child ::s/any :parent ::s/any)
  :ret boolean?)

(s/fdef clojure.core/special-symbol?
  :args (s/cat :s ::s/any)
  :ret boolean?)

(s/fdef clojure.core/reductions
    :args (s/cat
           :f (s/fspec
               :args (s/cat :acc ::s/any :x ::s/any)
               :ret ::s/any)
           :init (s/? ::s/any)
           :coll seqable?)
    :ret seq?)

(s/fdef clojure.core/reduced?
  :args (s/cat :x ::s/any)
  :ret boolean?)

(s/fdef clojure.core/sequence
  :args (s/alt
         :seq (s/cat :coll seqable?)
         :transducer (s/cat
                      :xform fn?
                      :colls (s/+ seqable?)))
  :ret seq?)

(s/fdef clojure.core/dec
  :args (s/cat :x number?)
  :ret number?)

(s/fdef clojure.core/inc
  :args (s/cat :x number?)
  :ret number?)

(s/fdef clojure.core/set
  :args (s/cat :coll coll?) ;TODO actually iterable
  :ret set?)

(s/fdef clojure.core/nfirst
  :args (s/cat :x seqable?)
  :ret seq?)

(s/fdef clojure.core/group-by
  :args (s/cat
         :f (s/fspec
             :args (s/cat :x ::s/any)
             :ret ::s/any)
         :coll seqable?)
  :ret (s/map-of ::s/any vector?))

(s/fdef clojure.core/keep
  :args (s/cat
         :f (s/fspec
             :args (s/cat :x ::s/any)
             :ret ::s/any)
         :coll (s/? seqable?))
  :ret (s/or
        :transducer fn?
        :seq seq?)
  :fn #(let [ret-type (-> % :ret first)]
         (if (-> % :args (contains? :coll))
           (= ret-type :seq)
           (= ret-type :transducer))))

(s/fdef clojure.core/keep-indexed
  :args (s/cat
         :f (s/fspec
             :args (s/cat
                     :i integer?
                     :x ::s/any)
             :ret ::s/any)
         :coll (s/? seqable?))
  :ret (s/or
        :transducer fn?
        :seq seq?)
  :fn #(let [ret-type (-> % :ret first)]
         (if (-> % :args (contains? :coll))
           (= ret-type :seq)
           (= ret-type :transducer))))

(s/fdef clojure.core/seqable?
  :args (s/cat :x ::s/any)
  :ret boolean?)

(s/fdef clojure.core/sort-by
  :args (s/cat
         :keyfn (s/fspec
                 :args (s/cat :x ::s/any)
                 :ret ::s/any)
         :comp (s/? (s/fspec
                     :args (s/cat :x ::s/any)
                     :ret number?))
         :coll seqable?) ;TODO actually iterable
  :ret seq?
  :fn #(= (count (-> % :args :coll))
          (count (:ret %))))

(s/fdef clojure.core/replicate
  :args (s/cat :n number? :x ::s/any)
  :ret seq?)

(s/fdef clojure.core/quot
  :args (s/cat :num number? :div number?)
  :ret number?)

(s/fdef clojure.core/partition
  :args (s/cat
         :n number?
         :step (s/? number?)
         :pad (s/? seqable?)
         :coll seqable?)
  :ret seq?)

(s/fdef clojure.core/name
  :args (s/cat
         :x (s/or
             :keyword? keyword?
             :string? string?
             :symbol? symbol?))
  :ret string?)

(s/fdef clojure.core/fnext
  :args (s/cat :x seqable?)
  :ret ::s/any)

;;TODO fns with inline forms don't get checked
(s/fdef clojure.core/rem
  :args (s/cat :num number? :div number?)
  :ret number?)

(s/fdef clojure.core/cat
  :args (s/cat :rf fn?)
  :ret fn?)

(s/fdef clojure.core/dedupe
  :args (s/cat :coll (s/? seqable?))
  :ret (s/or
        :transducer fn?
        :seq seq?)
  :fn #(let [ret-type (-> % :ret first)]
         (if (-> % :args (contains? :coll))
           (= ret-type :seq)
           (= ret-type :transducer))))

(s/fdef clojure.core/distinct
  :args (s/cat :coll (s/? seqable?))
  :ret (s/or
        :transducer fn?
        :seq seq?)
  :fn #(let [ret-type (-> % :ret first)]
         (if (-> % :args (contains? :coll))
          (= ret-type :seq)
          (= ret-type :transducer))))

(s/fdef clojure.core/eduction
  :args (s/cat
         :xforms (s/* fn?) ; actually transducers, can't specify
         :coll seqable?)
  :ret #(instance? clojure.core.Eduction %))

(s/fdef clojure.core/replace
  :args (s/cat
         :smap map?
         :coll (s/? seqable?))
  :ret (s/or
        :transducer fn?
        :vector vector?
        :seq seq?)
  :fn #(let [coll (-> % :args (get :coll ::not-found))
             [ret-type ret] (:ret %)]
         (cond
           (= coll ::not-found) (= ret-type :transducer)
           (not (vector? coll)) (= ret-type :seq)
           ;; so coll is a vector, can count
           :else (and (= ret-type :vector)
                      (= (count coll) (count ret))))))
