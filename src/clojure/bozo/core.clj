(ns
  ^{:doc "L-BFGS optimization for Clojure"
    :author "Antoine Choppin"}
  bozo.core
  (:import  [riso.numerical LBFGS]))

(def ^:private default-params
  {:maxit  200
   :m      5
   :eps    1.0e-5
   :xtol   1.0e-16
   :iprint [-1 0]})

(defn lbfgs
  "
  Finds a minimum of the function f, using the L-BFGS algorithm
  starting with an initial estimate x0.
  
  f is a function of x (double array) and must return:
  - the value of f at x
  - the derivatives of f wrt x at x
  
  The following parameters can be optionally specified:
    :maxit  maximum number of iterations (default 200)
    :m      number of corrections used in the BFGS update
            3 <= m <= 7 is recommended (default 5) 
    :eps    accuracy with which the solution is to be found (default 1.0e-5)
    :xtol   estimate of the machine precision (default 1.0e-16)
    :iprint ...
  Override the defaults like this:
    (lbfgs f x0 {:m 3})
  "
  [f ^doubles x0 & [params]]
  (let [x      (aclone x0)
        n      (count x)
        p      (merge default-params params)
        maxit  (:maxit p)
        m      (:m p)
        eps    (:eps p)
        xtol   (:xtol p)
        iprint (int-array (:iprint p))
        diagco false
        diag   (double-array (repeat n 0.0))
        iflag  (int-array [0])]
    (loop [it 0]
      (let [[fval gval] (f x)]
        (LBFGS/lbfgs n m x fval gval diagco diag iprint eps xtol iflag)
        (if (and (not= 0 (nth iflag 0)) (< it maxit))
          (recur (inc it))
          x)))))

