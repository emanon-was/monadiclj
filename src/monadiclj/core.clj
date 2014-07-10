(ns monadiclj.core
  (:import [clojure.lang Sequential]))

(defprotocol Functor
  (fmap [m f]))

(defn functor? [x] (satisfies? Functor x))

(defprotocol Monad
  (join [m])
  (bind [m f])
  (then [m ma]))

(defn monad? [x] (satisfies? Monad x))

(defrecord Unit [value]
  Functor
  (fmap [m f] (Unit. (f (:value m))))
  Monad
  (join [m]
    (let [a (:value m)]
      (if (monad? a)
        (join a)
        (Unit. a))))
  (bind [m f]  (join (fmap m f)))
  (then [m ma] (bind m (fn [x] ma))))

(defn unit?  [x] (instance? Unit x))
(defn unit   [x] (Unit. x))
(defn return [x] (unit x))

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

(extend-type Sequential
  Functor
  (fmap [m f] (map f m))
  Monad
  (join [m]
    (mapcat
     #(if (monad? %)
        (let [a (join %)]
          (cond (sequential? a) a
                (unit? a) (list (:value a))
                :else (list a)))
        (list %)) m))
  (bind [m f]  (join (fmap m f)))
  (then [m ma] (bind m (fn [x] ma))))

(defprotocol Maybe
  (just?    [m])
  (nothing? [m]))

(defn maybe? [m] (satisfies? Maybe m))

(defrecord Just [value])
(defrecord Nothing [value])

(extend-type Just
  Functor
  (fmap [m f] (try (Just. (f (:value m)))
                   (catch Exception e (Nothing. e))))
  Monad
  (join [m]
    (let [a (:value m)]
      (if (monad? a)
        (let [b (join a)]
          (cond (maybe? b) (join b)
                (unit? b) (Just. (:value b))
                :else (Just. a)))
        (Just. a))))
  (bind [m f]  (join (fmap m f)))
  (then [m ma] (bind m (fn [x] ma)))
  Maybe
  (just?    [m] true)
  (nothing? [m] false))

(extend-type Nothing
  Functor
  (fmap [m f] (Nothing. (:value m)))
  Monad
  (join [m] m)
  (bind [m f]  (join (fmap m f)))
  (then [m ma] m)
  Maybe
  (just?    [m] false)
  (nothing? [m] true))

(defmacro maybe-> [v & fs]
  `(>>= (Just. ~v) ~@fs))

