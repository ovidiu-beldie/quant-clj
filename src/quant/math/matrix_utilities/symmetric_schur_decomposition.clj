(ns quant.math.matrix-utilities.symmetric-schur-decomposition
  (:use [quant.math.matrix :only (count-rows, count-cols, get-main-diag, transpose, matrix, set-main-diag)]
        [incanter.core :only (abs, sqrt)]))

(def max-iter 100)
(def eps-prec 1E-15)

(declare main-loop, sort-eigens)

(defn sqr [x] (* x x))

(defn jacobi-rotate [m rot dil j1 k1 j2 k2]
  (do
  ;(prn "jacobi-rotate: j1=" j1 "k1=" k1 "j2=" j2 "k2=" k2)
  (let [x1 (get-in m [j1 k1]) 
        x2 (get-in m [j2 k2]) 
        ;_ (prn "m=" m "x1=" x1 "x2=" x2)
        new-x1 (- x1 (* dil (+ x2 (* x1 rot))))
        new-x2 (+ x2 (* dil (- x1 (* x2 rot))))
        upd-m (assoc-in m [j1 k1] new-x1)]
    (assoc-in upd-m [j2 k2] new-x2))))

(defn symmetric-schur-decomp [s]
  (if (or (zero? (count-rows s)) 
          (not (= (count-rows s) (count-cols s))))
    (throw (IllegalArgumentException. "Non-null square matrix required"))
    (let [size (count-rows s)
          diag (vec (get-main-diag s))
          _ (prn "symm-schur: size=" size "s=" s "diag=" diag)
          eigen-vecs (set-main-diag (matrix size size (repeat 0)) (repeat 1))
          _ (prn "symm-shcur: eigen-vecs=" eigen-vecs)
          ;tmp-acc (vec (repeat size 0))
          ;args-upd (main-loop {:ss s, :diag diag, :eigen-vecs eigen-vecs, :tmp-diag diag, :tmp-acc tmp-acc})
          args-upd (main-loop {:ss s, :diag diag, :eigen-vecs eigen-vecs, :tmp-diag diag})
          _ (prn "!!! symm-schur: ss=" (args-upd :ss) "diag=" (args-upd :diag) "eigen-vecs=" (args-upd :eigen-vecs) "args-upd=" args-upd)
          ]
      (sort-eigens args-upd))))
          

(defn zero-pred [ite smll d-j d-k]
  (do
  (prn "zero-pred: smll=" smll "eps-prec=" eps-prec "d-j=" d-j "d-k=" d-k "ite=" ite)
  (and  (> ite 5)
        (< smll (* eps-prec (abs d-j)))
        (< smll (* eps-prec (abs d-j))))))

(defn comp-tang [ss smll eps-prec heig j k]
  (do
  (prn "comp-tang: ss=" ss "j=" j "k=" k "heig=" heig)
  (if (< smll (* eps-prec (abs heig)))
    (/ (get-in ss [j k]) heig)
    (let [beta (/ heig 2 (get-in ss [j k]))
          tang (/ 1 (+ (abs beta) (sqrt (+ 1 (sqr beta)))))
          _ (prn "beta=" beta "tang=" tang)]
      (if (< beta 0) (- tang) tang)))))

(defn apply-delta [v incr-coll idx-set]
  (let [;_ (prn "apply-delta : v=" v "incr-coll=" incr-coll "idx-set=" idx-set)
        ordered-v (map vector v (range))
        init-vals (for [e ordered-v :when (contains? idx-set (second e))] (first e))
        new-vals (map + init-vals incr-coll)
        ;_ (prn "apply-delta :init-vals=" init-vals "new-vals=" new-vals)
        kvs (interleave idx-set new-vals)
        ;_ (prn "apply-delta : kvs=" kvs)
        ]
    (apply assoc v kvs)))
    
(defn jacobi-rotate-loop [rho sine m start stop arg-fn]
  (do
  ;(prn "start=" start "stop=" stop)
  ;(prn "jacobi-rotate-loop : arg-fn=" arg-fn)
  ;(prn "jacobi-rotate-loop : arg-fn=" arg-fn "(arg-fn 0)=" (arg-fn 0))
  (loop [l start, upd-m m]
    (if (= l stop)
      (do
      ;(prn "end of jacobi-rotate-loop: l=" l "stop=" stop)
      upd-m)
      (do
      ;(prn "jacobi-rotate-loop : calling jacobi-rotate")
      (recur (inc l) (apply jacobi-rotate upd-m rho sine (arg-fn l))))))))
 
(defmacro arguments [& args]
  args)

(defn apply-jacobi-rotate [ss rho sine eigen-vecs j k]
  (let [size (count-rows eigen-vecs)
        jrl (partial jacobi-rotate-loop rho sine)
        args [(list 0       j     (fn[l] (list l j l k)))
              (list (inc j) k     (fn[l] (list j l l k)))
              (list (inc k) size  (fn[l] (list j l k l)))]
        ss-upd  (loop [as args, new-ss ss]
                  (do
                  ;(prn "apply-jacobi: new-ss=" new-ss)
                  (if (nil? (first as))
                    new-ss
                    (recur (rest as) (apply jrl new-ss (first as))))))
        eig-upd (jrl eigen-vecs 0 size (fn [l] (list l j l k)))]
    {:ss ss-upd, :eigen-vecs eig-upd}))

(defn update [{:keys [ss diag eigen-vecs tmp-acc] :as args} smll j k]
  (let [;_ (prn "diag=" diag)
        heig (- (diag k) (diag j))
        tang (comp-tang ss smll eps-prec heig j k)
        cosin (/ 1 (sqrt (+ 1 (sqr tang))))
        sine (* tang cosin)
        rho (/ sine (inc cosin))
        upd-heig (* tang (get-in ss [j k]))
        _ (prn "update: smll=" smll "heig=" heig "tang=" tang "cosin=" cosin "sine=" sine "rho=" rho "upd-heig=" upd-heig)
        _ (prn "update: (tmp-acc j)=" (tmp-acc j) "(tmp-acc k)=" (tmp-acc k))
        tmp-acc-upd (apply-delta  tmp-acc  [(- upd-heig) upd-heig] #{j k})
        _ (prn "update: (tmp-acc-upd j)=" (tmp-acc-upd j) "(tmp-acc-upd k)=" (tmp-acc-upd k))
        _ (prn "update: (diag j)=" (diag j) "(diag k)=" (diag k))
        diag-upd    (apply-delta  diag     [(- upd-heig) upd-heig] #{j k})
        _ (prn "update: (diag-upd j)=" (diag-upd j) "(diag-upd k)=" (diag-upd k))
        ss-upd (assoc-in ss [j k] 0) 
        _ (prn "update : eigen-vecs before rotate=" eigen-vecs) 
        loop-res (apply-jacobi-rotate ss-upd rho sine eigen-vecs j k)
        _ (prn "update: ss=" (loop-res :ss) "eigen=" (loop-res :eigen-vecs))
        ]
    ;(assoc loop-res :tmp-acc tmp-acc-upd, :diag diag-upd)))
    (assoc args :ss (loop-res :ss), :eigen-vecs (loop-res :eigen-vecs), :tmp-acc tmp-acc-upd, :diag diag-upd)))

(defn j-iter [{:keys [ite threshold diag] :as arguments} j]
  (do
  (prn "--------------------------")
  (prn "j-iter, j=" j)
  (loop [k (inc j), arg arguments]
    (if (= k (count diag))
      arg
      (do
      (prn "--- k-iter, k=" k "---------")
      (let [diag (arg :diag)
            ss (arg :ss)
            _ (prn "j-iter: ss=" ss)
            smll (abs (get-in ss [j k]))
            _ (prn "j-iter: diag=" diag "ite=" ite "ss=" ss "j=" j "k=" k)
            upd-arg (if (zero-pred ite smll (diag j) (diag k))
                        (assoc arg :ss (assoc-in ss [j k] 0))
                        (if (> (abs (get-in ss [j k])) threshold)
                          (update arg smll j k)
                          arg))]
        (recur (inc k) upd-arg)))))))


(defn j-loop [{:keys [diag] :as arg}]
  ;for loop indexed by 'j'
  (let [;_ (prn "j-loop: diag=" diag)
        size (count-rows diag)]
    (loop [j 0, new-arg (assoc arg :tmp-acc (vec (repeat size 0)))]
      (do
      (prn "!!!! j-loop, new-arg=" new-arg)
      (if (= j (dec size))
        new-arg
        (recur (inc j) (j-iter new-arg j)))))))

(defn sum-row [r a]
  (let [upd-r (drop (inc a) r)
        _ (prn "sum-row: upd-r=" upd-r "r=" r "a=" a)]
    (reduce + (map abs upd-r))))

(defn comp-sum [ss]
  (let [_ (prn "comp-sum: ss=" ss)
        size (count-rows ss)]
    (loop [i 0, sum 0]
      (if (= i (dec size))
        sum
        (recur (inc i) (+ sum (sum-row (ss i) i)))))))

(defn main-iter [ite {:keys [ss tmp-diag] :as args}]
  (do
  (prn "*************************")
  (prn "main-iter, ite=" ite)
  (let [;_ (prn "main-iter: diag=" (args :diag))
        sum (comp-sum ss)
        size (count-rows ss)
        _ (prn "main-iter: sum=" sum)
        ]
    (if (zero? sum)
      (assoc args :keep-looking false)
      (let [thr (if (< ite 5) (/ sum (sqr size) 5) 0)
            args-upd (j-loop (assoc args :threshold thr :ite ite))
            upd-tmp-diag (vec (map + (args :tmp-diag) (args-upd :tmp-acc)))
            ;_ (prn "main-iter: upd-tmp-diag=" upd-tmp-diag)
            ]
        (assoc args-upd :tmp-diag upd-tmp-diag :diag upd-tmp-diag :keep-looking true))))))

(defn main-loop [args]
  (loop [ite 1, upd-args (assoc args :keep-looking true)]
    (do
    (prn "main-loop: upd-args=" upd-args)
    (if (or (= ite max-iter) (not (upd-args :keep-looking)))
      upd-args
      (recur (inc ite) (main-iter ite upd-args))))))

(defn sort-eigens [{:keys [eigen-vecs diag]}]
  (let [_ (prn "sort-eigens: eigen-vecs=" eigen-vecs "diag=" diag)
        pairs (sort-by first > (map vector diag (transpose eigen-vecs)))
        max-ev (ffirst pairs)
        diag-fn (fn [x] (if (< (abs (/ x max-ev)) 1E-16) 0 x)) 
        new-diag (map diag-fn (map first pairs))
        _ (prn "!!! sort-eigens: new-diag=" new-diag)
        sign-fn (fn [v] (if (< (first v) 0) (- 1) 1))
        vecs-from-pairs (map second pairs)
        signs (map sign-fn vecs-from-pairs)
        mult-sign-fn (fn [v s] (map #(* s %) v))
        new-eigen-vecs (transpose (map mult-sign-fn vecs-from-pairs signs))]
    {:diag new-diag, :eigen-vecs new-eigen-vecs}))
          
      
