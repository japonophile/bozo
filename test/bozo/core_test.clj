(ns bozo.core-test
  (:require [clojure.test :refer :all]
            [bozo.core :refer :all]))

(deftest test-lbfgs
  (testing "Example shipped with RISO"
    (let [f  (fn [^doubles x]
               (let [n (alength x)]
                 (loop [f0 0, g0 [] i 0]
                   (let [x1 (aget x i)
                         x2 (aget x (inc i))
                         t1 (- 1.0 x1)
                         t2 (* 10.0 (- x2 (* x1 x1)))
                         g12 (* 20.0 t2)
                         g11 (* -2.0 (+ (* x1 g12) t1))
                         f1 (+ f0 (* t1 t1) (* t2 t2))
                         g1  (concat g0 [g11 g12])]
                     (if (< (+ i 2) n)
                       (recur f1 g1 (+ i 2))
                       [(double f1) (double-array g1)])))))
          x0 (double-array (apply concat (repeat 50 [-1.2 1.0])))
          x  (lbfgs f x0 {:iprint [1 0]})]
      (is (= (vec (apply concat (repeat 50 [0.9999999520726296 0.9999998997951525])))
             (vec x))))))

