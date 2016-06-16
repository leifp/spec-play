(ns spec.clojure.spec
  (:require [clojure.spec :as s]))

(use 'clojure.repl)

(alias 'c 'clojure.core)

;;;; spec examples

;;; define, a predicate or set is a spec
(s/def ::date inst?)
(s/def ::suit #{:club :diamond :heart :spade})

;;; combinators: and, or
(s/def ::big-even (s/and integer? even? #(> % 1000)))
(s/def ::name-or-id (s/or :name string?
                          :id   integer?))

;;; combinators: regex

(s/def ::ingredient (s/cat :quantity number? :unit keyword?))
(s/def ::seq-of-keywords (s/* keyword?))
(s/def ::odds-then-maybe-even (s/cat :odds (s/+ odd?)
                                     :even (s/? even?)))
(s/def ::opts (s/* (s/cat :opt keyword? :val boolean?)))
(s/def ::config (s/*
                  (s/cat :prop string?
                         :val  (s/alt :s string? :b boolean?))))
;; side constraints
(s/def ::even-strings (s/& (s/* string?) #(even? (count %))))

(s/def ::nested
  (s/cat :names-kw #{:names}
         :names (s/spec (s/* string?))
         :nums-kw #{:nums}
         :nums (s/spec (s/* number?))))
(s/conform ::nested [:names ["a" "b"] :nums [1 2 3]])
(s/def ::unnested
  (s/cat :names-kw #{:names}
         :names (s/* string?)
         :nums-kw #{:nums}
         :nums (s/* number?)))
(s/conform ::unnested [:names "a" "b" :nums 1 2 3])

;;; maps

(def email-regex #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$")
(s/def ::email-type (s/and string? #(re-matches email-regex %)))

(s/def ::acctid integer?)
(s/def ::first-name string?)
(s/def ::last-name string?)
(s/def ::email ::email-type)

(s/def ::person (s/keys :req [::first-name ::last-name ::email]
                        :opt [::phone]))

(s/def :unq/person
  (s/keys :req-un [::first-name ::last-name ::email]
          :opt-un [::phone]))

(s/def :keys/and-or (s/keys :req [::x ::y (or ::secret (and ::user ::pwd))] :opt [::z]))

;;; multi-spec

(s/def :event/type keyword?)
(s/def :event/timestamp integer?)
(s/def :search/url string?)
(s/def :error/message string?)
(s/def :error/code integer?)

(defmulti event-type :event/type)
(defmethod event-type :event/search [_]
  (s/keys :req [:event/type :event/timestamp :search/url]))
(defmethod event-type :event/error [_]
  (s/keys :req [:event/type :event/timestamp :error/message :error/code]))

(s/def :event/event (s/multi-spec event-type :event/type))

(s/valid? :event/event
  {:event/type :event/search
   :event/timestamp 1463970123000
   :search/url "http://clojure.org"})
(s/valid? :event/event
  {:event/type :event/error
   :event/timestamp 1463970123000
   :error/message "Invalid host"
   :error/code 500})

;;; collections

;; NB: these don't check type of coll
(s/def ::kw-vec (s/coll-of keyword? []))
(s/def ::number-set (s/coll-of number? #{}))

(s/def ::point (s/tuple double? double? double?))

(s/def ::scores (s/map-of string? integer?))

;;; fdef, fspec
(defn ranged-rand
  "Returns random integer in range start <= rand < end"
  [start end]
  (+ start (rand-int (- end start))))

(s/fdef ranged-rand
  :args (s/and (s/cat :start integer? :end integer?)
               #(< (:start %) (:end %)))
  :ret integer?
  :fn (s/and #(>= (:ret %) (-> % :args :start))
             #(< (:ret %) (-> % :args :end))))

(defn adder [x] #(+ x %))
(s/fdef adder
  :args (s/cat :x number?)
  :ret (s/fspec :args (s/cat :y number?)
                :ret number?)
  :fn #(= (-> % :args :x) ((:ret %) 0)))

;;; macros
(s/fdef clojure.core/declare
    :args (s/cat :names (s/* symbol?))
    :ret ::s/any)

;;; extended example


(def suit? #{:club :diamond :heart :spade})
(def rank? (into #{:jack :queen :king :ace} (range 2 11)))
(def deck (for [suit suit? rank rank?] [rank suit]))

(s/def ::card (s/tuple rank? suit?))
(s/def ::hand (s/* ::card))

(s/def ::name string?)
(s/def ::score integer?)
(s/def ::player (s/keys :req [::name ::score ::hand]))

(s/def ::players (s/* ::player))
(s/def ::deck (s/* ::card))
(s/def ::game (s/keys :req [::players ::deck]))


;;;; spec spec

;; TODO
(s/def ::s/req (s/+ (s/alt :simple keyword?
                           :complex (s/spec (s/cat :op '#{and or}
                                                   :args (s/+ ::s/req))))))
(s/def ::s/req-un (s/* keyword?))
(s/def ::s/opt    (s/* keyword?))
(s/def ::s/opt-un (s/* keyword?))

(s/def ::s/keys--args
  (s/keys* :opt-un [::s/req ::s/req-un ::s/opt ::s/opt-un ::s/gen]))

(s/def :keys/and-or (s/keys :req [::x ::y (or ::secret (and ::user ::pwd))] :opt [::z]))

;; (s/fdef s/keys
;;   :args ::s/keys--args
;;   :ret ::s/any)

;; (s/unstrument #'clojure.spec/keys)

;;TODO distinguish preds, regex specs?
(s/def ::s/spec-spec
  (s/or :simple ::s/simple-spec
        :complex ::s/complex-spec))

(s/def ::s/simple-spec
  (s/or :named-fn symbol?
        :set set?
        :keyword keyword?))

(defmulti spec-op first)
(defmethod spec-op 'fn*             [_] ::s/any)
(defmethod spec-op `c/fn [_] ::s/any)
(defmethod spec-op `s/spec [_]
  (s/cat :op `#{s/spec}
         :args ::s/spec-spec))
(defmethod spec-op `s/and [_]
  (s/cat :op `#{s/and}
         :args (s/* ::s/spec-spec)))
(defmethod spec-op `s/or [_]
  (s/cat :op `#{s/or}
         :args (s/* (s/cat :tag keyword? :spec ::s/spec-spec))))
(defmethod spec-op `s/& [_]
  (s/cat :op `#{s/&}
         :args (s/cat :regex ::s/spec-spec  ;TODO
                      :preds (s/* ::s/spec-spec))))
(defmethod spec-op `s/* [_]
  (s/cat :op `#{s/*} :pred ::s/spec-spec))
(defmethod spec-op `s/+ [_]
  (s/cat :op `#{s/+} :pred ::s/spec-spec))
(defmethod spec-op `s/? [_]
  (s/cat :op `#{s/?} :pred ::s/spec-spec))
(defmethod spec-op `s/cat [_]
  (s/cat :op `#{s/cat}
         :args (s/* (s/cat :tag keyword? :spec ::s/spec-spec))))
(defmethod spec-op `s/alt [_]
  (s/cat :op `#{s/alt}
         :args (s/* (s/cat :tag keyword? :spec ::s/spec-spec))))
(defmethod spec-op `s/keys [_]
  (s/cat :op `#{s/keys}
         :args ::s/keys--args))
(defmethod spec-op `s/keys* [_]
  (s/cat :op `#{s/keys*}
         :args ::s/keys--args))
(defmethod spec-op `s/coll-of [_]
  (s/cat :op `#{s/coll-of}
         :args (s/cat :pred ::s/spec-spec :init-coll ::s/any)))
(defmethod spec-op `s/tuple [_]
  (s/cat :op `#{s/tuple}
         :args (s/* ::s/spec-spec)))
(defmethod spec-op `s/map-of [_]
  (s/cat :op `#{s/map-of}
         :args (s/cat :kpred ::s/spec-spec :vpred ::s/spec-spec)))
(defmethod spec-op `s/multi-spec [_]
  (s/cat :op `#{s/multi-spec}
         :args (s/cat :mm symbol? :retag symbol?))) ;TODO

(s/def ::s/complex-spec (s/multi-spec spec-op conj))

