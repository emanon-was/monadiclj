(ns monadiclj.core
  (:import [clojure.lang Sequential]))

(defprotocol Functor
  (fmap [m f]))

(defn functor? [x] (satisfies? Functor x))

(defprotocol Monoid
  (mempty  [m])
  (mappend [ma mb])
  (mconcat [m]))

(defn monoid? [x] (satisfies? Monoid x))

(defrecord Empty [])

(defprotocol Monad
  (join [m])
  (bind [m f])
  (then [m ma]))

(defn monad? [x] (satisfies? Monad x))

(defprotocol MonadPlus
  (mzero [m])
  (mplus [ma mb]))

(defrecord Unit [_])

(defn unit? [x] (instance? Unit x))
(defn unit-empty? [x] (and (unit? x) (instance? Empty (:_ x))))

(defn return
  ([]    (Unit. (Empty.)))
  ([x]   (Unit. x))
  ([_ x] (Unit. x)))

(extend-type Unit
  Monoid
  (mempty  [m] (Unit. (Empty.)))
  (mappend [ma mb]
    (cond (= (instance? Empty (:_ ma))) mb
          (= (instance? Empty (:_ mb))) ma
          :else (Unit. (mappend (:_ ma) (:_ mb)))))
  (mconcat [m] (mappend m (mempty m)))
  Functor
  (fmap [m f]
    (if (instance? Empty (:_ m)) m
        (Unit. (f (:_ m)))))
  Monad
  (join [m]
    (let [a (:_  m)]
      (if (monad? a)
        (join a)
        (Unit. a))))
  (bind [m f]  (join (fmap m f)))
  (then [m ma] (bind m (fn [x] ma))))

(defmacro >>= [m & fs]
  `(-> ~m ~@(map (fn [f]
                   (if (list? f)
                     `(bind #(~(first f) % ~@(rest f)))
                     `(bind ~f)))
                 fs)))

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

(defn guard [exp]
  (if exp (return) (return nil)))

(extend-type Sequential
  Functor
  (fmap [m f] (map f m))
  Monoid
  (mempty  [m] '())
  (mappend [ma mb]
    (apply concat (list ma (cond (sequential? mb) mb
                                 (unit? mb) (if (= (unit) mb) '() (list (:_ mb)))
                                 :else (list mb)))))
  (mconcat [m] (mappend m (mempty m)))
  Monad
  (join [m]
    (mapcat
     #(if (monad? %)
        (let [a (join %)]
          (cond (sequential? a) a
                (unit-empty? a) '()
                (unit? a) (list (:_ a))
                :else (list a)))
        (list %)) m))
  (bind [m f]  (join (fmap m f)))
  (then [m ma] (bind m (fn [x] ma))))

(defprotocol Maybe
  (just?    [m])
  (nothing? [m]))

(defn maybe? [m] (satisfies? Maybe m))

(defrecord Just [_])
(defrecord Nothing [_])

(extend-type Just
  Functor
  (fmap [m f] (try (Just. (f (:_ m)))
                   (catch Exception e (Nothing. e))))
  Monoid
  (mempty  [m] (Nothing. nil))
  (mappend [ma mb]
    (let [mc (cond (maybe? mb) mb
                   (unit? mb)  (if (= (unit) mb) (Nothing. nil) (Just. (:_ mb)))
                   :else (Just. mb))]
      (if (nothing? mc) ma (Just. (mappend (:_ ma) (:_ mc))))))
  (mconcat [m] (mappend m (mempty m)))
  Monad
  (join [m]
    (let [a (:_ m)]
      (if (monad? a)
        (let [b (join a)]
          (cond (maybe? b) (join b)
                (unit-empty? b) (Nothing. nil)
                (unit? b) (Just. (:_ b))
                :else (Just. a)))
        (Just. a))))
  (bind [m f]  (join (fmap m f)))
  (then [m ma] (bind m (fn [x] ma)))
  Maybe
  (just?    [m] true)
  (nothing? [m] false))

(extend-type Nothing
  Functor
  (fmap [m f] (Nothing. (:_ m)))
  Monoid
  (mempty  [m] (Nothing. nil))
  (mappend [ma mb]
    (let [mc (cond (maybe? mb) mb
                   (unit-empty? mb) (Nothing. nil)
                   (unit? mb) (Just. (:_ mb))
                   :else (Just. mb))]
      (if (nothing? mc) ma mc)))
  (mconcat [m] (mappend m (mempty m)))
  Monad
  (join [m] m)
  (bind [m f]  (join (fmap m f)))
  (then [m ma] m)
  Maybe
  (just?    [m] false)
  (nothing? [m] true))

(defmacro maybe-> [v & fs]
  `(>>= (Just. ~v) ~@fs))

