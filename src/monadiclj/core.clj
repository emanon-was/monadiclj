(ns monadiclj.core
  (:import [clojure.lang Sequential]))

(defprotocol Functor
  (fmap [fa] [fa g]))

(defprotocol Monad
  (join   [ma])
  (return [ma] [ma b])
  (bind   [ma] [ma f])
  (then   [ma mb]))

(defprotocol MonadPlus
  (mzero [ma]))

(defn functor?    [ma] (satisfies? Functor ma))
(defn monad?      [ma] (satisfies? Monad ma))
(defn monad-plus? [ma] (satisfies? MonadPlus ma))

(defn functor-context [fa]
  (let [context {:fmap ::this-functor-does-not-define-fmap}]
    (if (functor? fa)
      (assoc context :fmap (fmap fa))
      context)))

(defn monad-context [ma]
  (let [context {:bind   ::this-monad-does-not-define-bind,
                 :return ::this-monad-does-not-define-return}]
    (if (monad? ma)
      (assoc context :bind (bind ma) :return (return ma))
      context)))

(defn monad-plus-context [ma]
  (let [context {:mzero ::this-monad-plus-does-not-define-mzero}]
    (if (monad-plus? ma)
      (assoc context :mzero (mzero ma))
      context)))

(defn contexts [ma]
  (conj {}
        (functor-context ma)
        (monad-context ma)
        (monad-plus-context ma)))

(defprotocol Maybe
  (just?    [ma])
  (nothing? [ma]))

(defn maybe? [ma] (satisfies? Maybe ma))

(defrecord Just [:-])
(defrecord Nothing [:-])

(extend-type Just
  Functor
  (fmap
    ([fa g] (Just. (g (:= fa))))
    ([fa]   (fn [g] (fmap fa g))))
  Monad
  (join [ma]
    (let [a (:= ma)]
      (if (maybe? a) (join a) ma))))
  
;;   (bind [m f]  (join (fmap m f)))
;;   (then [ma mb] (bind ma (fn [a] (bind mb (fn [b] (return b))))))
;;   MonadPlus
;;   (mzero [m] (mempty m))
;;   (mplus [ma mb] (mappend ma mb))
;;   Maybe
;;   (just?    [m] true)
;;   (nothing? [m] false))

;; (extend-type Nothing
;;   Monoid
;;   (mempty  [m] (Nothing. nil))
;;   (mappend [ma mb]
;;     (let [mc (cond (maybe?     mb) mb
;;                    (empty-set? mb) (Nothing. nil)
;;                    (unit-set?  mb) (Just. (:<- mb))
;;                    :else (Just. mb))]
;;       (if (nothing? mc) ma mc)))
;;   (mconcat [m] (mappend m (mempty m)))
;;   Functor
;;   (fmap [m f] (Nothing. (:<- m)))
;;   Monad
;;   (join [m] m)
;;   (bind [m f]  (join (fmap m f)))
;;   (then [m ma] m)
;;   MonadPlus
;;   (mzero [m] (mempty m))
;;   (mplus [ma mb] (mappend ma mb))
;;   Maybe
;;   (just?    [m] false)
;;   (nothing? [m] true))

;; (defmacro maybe-> [v & fs]
;;   `(>>= (Just. ~v) ~@fs))



;; (defmacro >>= [m & fs]
;;   (let [argsym '_]
;;     `(-> ~m ~@(map (fn [f]
;;                      (if (list? f)
;;                        `(bind (fn [~argsym] ~f))
;;                        `(bind ~f))) fs))))

;; (defmacro >> [m & ms]
;;   `(-> ~m ~@(map (fn [m] `(then ~m)) ms)))

;; (defmacro ^{:private true} assert-args [fnname & pairs]
;;   `(do (when-not ~(first pairs)
;;          (throw (IllegalArgumentException.
;;                  ~(str fnname " requires " (second pairs)))))
;;        ~(let [more (nnext pairs)]
;;           (when more
;;             (list* `assert-args fnname more)))))

;; (defmacro domonad [bindings & body]
;;   (assert-args
;;    domonad
;;    (vector? bindings) "a vector for its binding"
;;    (even? (count bindings)) "an even number of forms in binding vector")
;;   (let [var  (first   bindings)
;;         val  (second  bindings)
;;         next (nthrest bindings 2)]
;;     (if (seq next)
;;       `(bind ~val
;;              (fn [~var]
;;                (domonad [~@next] ~@body)))
;;       `(bind ~val
;;              (fn [~var]
;;                ~@body)))))

;; (defrecord UnitSet [<-] Semigroup)
;; (defrecord EmptySet [] Semigroup)

;; (defn unit-set?  [x] (instance? UnitSet x))
;; (defn empty-set? [x] (instance? EmptySet x))

;; (extend-type UnitSet
;;   Monoid
;;   (mempty  [m] (EmptySet.))
;;   (mappend [ma mb]
;;     (cond (empty-set? mb) ma
;;           (unit-set?  mb) (UnitSet. (mappend (:<- ma) (:<- mb)))))
;;   (mconcat [m] (mappend m (mempty m)))
;;   Functor
;;   (fmap [m f] (UnitSet. (f (:<- m))))
;;   Monad
;;   (join [m] (let [a (:<-  m)]
;;               (if (monad? a) (join a) (UnitSet. a))))
;;   (bind [m f]  (join (fmap m f)))
;;   (then [ma mb] (bind ma (fn [a] (bind mb (fn [b] (return b))))))
;;   MonadPlus
;;   (mzero [m] (mempty m))
;;   (mplus [ma mb] (mappend ma mb)))

;; (extend-type EmptySet
;;   Monoid
;;   (mempty  [m] (EmptySet.))
;;   (mappend [ma mb]
;;     (cond (empty-set? mb) ma
;;           (unit-set?  mb) mb))
;;   (mconcat [m] (mappend m (mempty m)))
;;   Functor
;;   (fmap [m f] m)
;;   Monad
;;   (join [m] m)
;;   (bind [m f]  (join (fmap m f)))
;;   (then [ma mb] ma)
;;   MonadPlus
;;   (mzero [m] (mempty m))
;;   (mplus [ma mb] (mappend ma mb)))

;; (defn return
;;   ([]    (EmptySet.))
;;   ([x]   (UnitSet. x))
;;   ([_ x] (UnitSet. x)))

;; (defn guard
;;   ([exp] (if exp (return) (return nil)))
;;   ([exp x] (if exp (return) (return x))))

;; (extend-type Sequential
;;   Monoid
;;   (mempty  [m] '())
;;   (mappend [ma mb]
;;     (apply concat
;;            (list ma
;;                  (cond (sequential? mb) mb
;;                        (empty-set?  mb) '()
;;                        (unit-set?   mb) (list (:<- mb))
;;                        :else (list mb)))))
;;   (mconcat [m] (mappend m (mempty m)))
;;   Functor
;;   (fmap [m f] (map f m))
;;   Monad
;;   (join [m]
;;     (mapcat
;;      #(if (monad? %)
;;         (let [a (join %)]
;;           (cond (sequential? a) a
;;                 (empty-set?  a) '()
;;                 (unit-set?   a) (list (:<- a))
;;                 :else (list a)))
;;         (list %)) m))
;;   (bind [m f]  (join (fmap m f)))
;;   (then [ma mb] (bind ma (fn [a] (bind mb (fn [b] (return b))))))
;;   MonadPlus
;;   (mzero [m] (mempty m))
;;   (mplus [ma mb] (mappend ma mb)))

;; (defprotocol Maybe
;;   (just?    [m])
;;   (nothing? [m]))

;; (defn maybe? [m] (satisfies? Maybe m))

;; (defrecord Just [<-])
;; (defrecord Nothing [<-])

;; (extend-type Just
;;   Monoid
;;   (mempty  [m] (Nothing. nil))
;;   (mappend [ma mb]
;;     (let [mc (cond (maybe?     mb) mb
;;                    (empty-set? mb) (Nothing. nil)
;;                    (unit-set?  mb) (Just. (:<- mb))
;;                    :else (Just. mb))]
;;       (if (nothing? mc) ma (Just. (mappend (:<- ma) (:<- mc))))))
;;   (mconcat [m] (mappend m (mempty m)))
;;   Functor
;;   (fmap [m f] (try (Just. (f (:<- m)))
;;                    (catch Exception e (Nothing. e))))
;;   Monad
;;   (join [m] (let [a (:<- m)]
;;               (if (monad? a)
;;                 (let [b (join a)]
;;                   (cond (maybe?     b) (join b)
;;                         (empty-set? b) (Nothing. nil)
;;                         (unit-set?  b) (Just. (:<- b))
;;                         :else (Just. b)))
;;                 (Just. a))))
;;   (bind [m f]  (join (fmap m f)))
;;   (then [ma mb] (bind ma (fn [a] (bind mb (fn [b] (return b))))))
;;   MonadPlus
;;   (mzero [m] (mempty m))
;;   (mplus [ma mb] (mappend ma mb))
;;   Maybe
;;   (just?    [m] true)
;;   (nothing? [m] false))

;; (extend-type Nothing
;;   Monoid
;;   (mempty  [m] (Nothing. nil))
;;   (mappend [ma mb]
;;     (let [mc (cond (maybe?     mb) mb
;;                    (empty-set? mb) (Nothing. nil)
;;                    (unit-set?  mb) (Just. (:<- mb))
;;                    :else (Just. mb))]
;;       (if (nothing? mc) ma mc)))
;;   (mconcat [m] (mappend m (mempty m)))
;;   Functor
;;   (fmap [m f] (Nothing. (:<- m)))
;;   Monad
;;   (join [m] m)
;;   (bind [m f]  (join (fmap m f)))
;;   (then [m ma] m)
;;   MonadPlus
;;   (mzero [m] (mempty m))
;;   (mplus [ma mb] (mappend ma mb))
;;   Maybe
;;   (just?    [m] false)
;;   (nothing? [m] true))

;; (defmacro maybe-> [v & fs]
;;   `(>>= (Just. ~v) ~@fs))

