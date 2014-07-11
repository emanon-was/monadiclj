(ns monadiclj.core
  (:import [clojure.lang Sequential]))

(defprotocol Semigroup)

(defprotocol Monoid
  (mempty  [m])
  (mappend [ma mb])
  (mconcat [m]))

(defprotocol Functor
  (fmap [m f]))

(defprotocol Monad
  (join [m])
  (bind [m f])
  (then [m ma]))

(defprotocol MonadPlus
  (mzero [m])
  (mplus [ma mb]))

(defn semigroup? [x] (satisfies? Semigroup x))
(defn monoid?    [x] (satisfies? Monoid x))
(defn functor?   [x] (satisfies? Functor x))
(defn monad?     [x] (satisfies? Monad x))

(defrecord UnitSet [=] Semigroup)
(defrecord EmptySet [] Semigroup)

(defn unit-set?  [x] (instance? UnitSet x))
(defn empty-set? [x] (instance? EmptySet x))

(extend-type UnitSet
  Monoid
  (mempty  [m] (EmptySet.))
  (mappend [ma mb]
    (cond (empty-set? mb) ma
          (unit-set?  mb) (UnitSet. (mappend (:= ma) (:= mb)))))
  (mconcat [m] (mappend m (mempty m)))
  Functor
  (fmap [m f] (UnitSet. (f (:= m))))
  Monad
  (join [m] (let [a (:=  m)]
              (if (monad? a) (join a) (UnitSet. a))))
  (bind [m f]  (join (fmap m f)))
  (then [m ma] (bind m (fn [x] ma)))
  MonadPlus
  (mzero [m] (mempty m))
  (mplus [ma mb] (mappend ma mb)))

(extend-type EmptySet
  Monoid
  (mempty  [m] (EmptySet.))
  (mappend [ma mb]
    (cond (empty-set? mb) ma
          (unit-set?  mb) mb))
  (mconcat [m] (mappend m (mempty m)))
  Functor
  (fmap [m f] m)
  Monad
  (join [m] m)
  (bind [m f]  (join (fmap m f)))
  (then [m ma] (bind m (fn [x] ma)))
  MonadPlus
  (mzero [m] (mempty m))
  (mplus [ma mb] (mappend ma mb)))

(defn return
  ([]    (EmptySet.))
  ([x]   (UnitSet. x))
  ([_ x] (UnitSet. x)))

(defmacro >>= [m & fs]
  `(-> ~m ~@(map (fn [f] (if (list? f)
                           `(bind #(~(first f) % ~@(rest f)))
                           `(bind ~f))) fs)))

(defmacro >> [m & ms]
  `(-> ~m ~@(map (fn [m] `(then ~m)) ms)))

(defmacro domonad [bindings & body]
  (let [next (nthrest bindings 2)]
    (if (seq next)
      `(bind ~(second bindings)
             (fn [~(first bindings)]
               (domonad [~@next] ~@body)))
      `(bind ~(second bindings)
             (fn [~(first bindings)]
               ~@body)))))

(defn guard
  ([exp] (if exp (return) (return nil)))
  ([exp x] (if exp (return) (return x))))

(extend-type Sequential
  Monoid
  (mempty  [m] '())
  (mappend [ma mb]
    (apply concat
           (list ma
                 (cond (sequential? mb) mb
                       (empty-set?  mb) '()
                       (unit-set?   mb) (list (:= mb)))
                 :else (list mb))))
  (mconcat [m] (mappend m (mempty m)))
  Functor
  (fmap [m f] (map f m))
  Monad
  (join [m]
    (mapcat
     #(if (monad? %)
        (let [a (join %)]
          (cond (sequential? a) a
                (empty-set?  a) '()
                (unit-set?   a) (list (:= a))
                :else (list a)))
        (list %)) m))
  (bind [m f]  (join (fmap m f)))
  (then [m ma] (bind m (fn [x] ma)))
  MonadPlus
  (mzero [m] (mempty m))
  (mplus [ma mb] (mappend ma mb)))

(defprotocol Maybe
  (just?    [m])
  (nothing? [m]))

(defn maybe? [m] (satisfies? Maybe m))

(defrecord Just [=])
(defrecord Nothing [=])

(extend-type Just
  Monoid
  (mempty  [m] (Nothing. nil))
  (mappend [ma mb]
    (let [mc (cond (maybe?     mb) mb
                   (empty-set? mb) (Nothing. nil)
                   (unit-set?  mb) (Just. (:= mb))
                   :else (Just. mb))]
      (if (nothing? mc) ma (Just. (mappend (:= ma) (:= mc))))))
  (mconcat [m] (mappend m (mempty m)))
  Functor
  (fmap [m f] (try (Just. (f (:= m)))
                   (catch Exception e (Nothing. e))))
  Monad
  (join [m] (let [a (:= m)]
              (if (monad? a)
                (let [b (join a)]
                  (cond (maybe?     b) (join b)
                        (empty-set? b) (Nothing. nil)
                        (unit-set?  b) (Just. (:= b))
                        :else (Just. b)))
                (Just. a))))
  (bind [m f]  (join (fmap m f)))
  (then [m ma] (bind m (fn [x] ma)))
  MonadPlus
  (mzero [m] (mempty m))
  (mplus [ma mb] (mappend ma mb))
  Maybe
  (just?    [m] true)
  (nothing? [m] false))

(extend-type Nothing
  Monoid
  (mempty  [m] (Nothing. nil))
  (mappend [ma mb]
    (let [mc (cond (maybe?     mb) mb
                   (empty-set? mb) (Nothing. nil)
                   (unit-set?  mb) (Just. (:= mb))
                   :else (Just. mb))]
      (if (nothing? mc) ma mc)))
  (mconcat [m] (mappend m (mempty m)))
  Functor
  (fmap [m f] (Nothing. (:= m)))
  Monad
  (join [m] m)
  (bind [m f]  (join (fmap m f)))
  (then [m ma] m)
  MonadPlus
  (mzero [m] (mempty m))
  (mplus [ma mb] (mappend ma mb))
  Maybe
  (just?    [m] false)
  (nothing? [m] true))

(defmacro maybe-> [v & fs]
  `(>>= (Just. ~v) ~@fs))

