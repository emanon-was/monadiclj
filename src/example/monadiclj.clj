(ns example.monadiclj
  (:use    [monadiclj.core])
  (:import [monadiclj.core Just Nothing]))

;; ========================
;;  List Monad
;; ========================

(>>= '(1 2 3 4) (+ 5) (+ 6))

;; (12 13 14 15)


(-> '(1 2 3 4) (>>= return))

;; (1 2 3 4)

(-> '(1 2 3 4) (>>= (+ 5)))

;; (6 7 8 9)

(-> '(1 2 3 4) (>>= #(return (+ % 5))))

;; (6 7 8 9)

(-> '(1 2 3 4) (>>= #(list % (+ % 5))))

;; (1 6 2 7 3 8 4 9)

(-> '(1 2 3 4) (>>= #(return (list % (+ % 5)))))

;; (1 6 2 7 3 8 4 9)

(domonad [x '(1 2 3)
          y '(4 5 6)]
         (+ x y))

;; (5 6 7 6 7 8 7 8 9)


(domonad [x '(1 2 3)
          y '(4 5 6)]
         (return (+ x y)))

;; (5 6 7 6 7 8 7 8 9)

(domonad [x '(1 2 3)
          y '(4 5 6)]
         (>> (guard (or (= x 3) (= y 6)))
             (return (+ x y))))

;;(5 6 6 7)

(domonad [x '(1 2 3)
          y '(4 5 6)]
         (guard (or (= x 3) (= y 6))
                (return (+ x y))))

;;(5 6 6 7)

;; ========================
;;  Maybe Monad
;; ========================

(>>= (Just. 1) (+ 2) (+ 3))

;; #monadiclj.core.Just{:value 6}

(>>= (Just. 1) (/ 0) (+ 3))

;; #monadiclj.core.Nothing{:value #<ArithmeticException java.lang.ArithmeticException: Divide by zero>}

(-> (Just. 1) (>>= return))

;; #monadiclj.core.Just{:value 1}

(-> (Just. 1) (>>= (+ 2)))

;; #monadiclj.core.Just{:value 3}

(-> (Just. 1) (>>= #(return (+ % 2))))

;; #monadiclj.core.Just{:value 3}

(-> (Just. 1) (>>= #(return (/ % 0))))

;; #monadiclj.core.Nothing{:value #<ArithmeticException java.lang.ArithmeticException: Divide by zero>}

(>> (Just. 1) (Just. 2))

;; #monadiclj.core.Just{:value 2}

(>> (Just. 1) (Nothing. nil))

;; #monadiclj.core.Nothing{:value nil}

(>> (Nothing. nil) (Just. 1))

;; #monadiclj.core.Nothing{:value nil}

(-> (Just. 1) (>>= (+ 2)) (>> (Nothing. nil)) (>>= (+ 3)))

;; #monadiclj.core.Nothing{:value nil}


(domonad [x (Just. 1)
          y (Just. 2)]
         (+ x y))

;; #monadiclj.core.Just{:value 3}

(domonad [x (Just. 1)
          y (Just. 2)]
         (return (+ x y)))

;; #monadiclj.core.Just{:value 3}



