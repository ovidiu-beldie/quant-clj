; Copyright (c) 2012 Ovidiu Beldie. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.
; The initial QuantLib library, Clojure and any other open source software 
; referenced in this library are the propriety of their respective owners

(ns quant.math.integrals.kronrod
  (:use
   [quant.common :only (half)]
   [quant.defines :only (epsilon min-pos-real)]
   [quant.math.integrals.core]
   [incanter.core :only (abs, pow)]))

(declare loop-10-21, comp-resasc, formula, convergent?, rescale-err,
         form-higher-order, integ-recur)
(declare x1 x2 x3 x4 w10 w21a w21b w43a w43b w87a w87b g7w k15w k15t)

;;; non-adaptive

(defn kronrod-non-adaptive [abs-acc max-ev rel-acc]
  (reify Integrator
    (integrate [_ f a b]
      (let [hl (half (- b a))
            center (half (+ b a))
            nb-evals [21, 43, 87]]
        (loop [i 0, old-p nil]
          (let [p (formula (nb-evals i) f hl center old-p)]
            (if (convergent? abs-acc rel-acc p)
              (:res p)
              (recur (inc i) p))))))))

(defn formula-dispatch [nb-evals f hl center p]
  nb-evals)

(defmulti formula formula-dispatch)

;; 10 and 21 points formula

(defmethod formula 21 [nb-ev f hl center _]
  (let [fc (f center)
        lp (partial loop-10-21 f hl center)
        {r10 :r10, r21-a :r21, r-abs-a :r-abs, fv-a :fv :as la} (lp x1 w21a)
        {r21-b :r21, r-abs-b :r-abs, fv-b :fv :as lb} (lp x2 w21b)
        r21 (+ (* (w21b 5) fc) r21-a r21-b)
        r-abs (+ (* (w21b 5) (abs fc)) r-abs-a r-abs-b)
        savfun (into fv-a fv-b)
        mean (half r21)
        r-asc (+ (* (w21b 5) (abs (- fc mean)))
                 (comp-resasc la lb mean))
        err (rescale-err (* (- r21 r10) hl)
                          {:r-abs (* r-abs hl), :r-asc r-asc})]
    {:savfun savfun, :r-abs (* r-abs hl), :r-asc (* r-asc hl),
      :res (* r21 hl), :err err, :nb-ev nb-ev}))
        
(defn loop-10-21 [f hl c x w21]
  (let [absc (map #(* hl %) x)
        fv1 (map #(f (+ c %)) absc)
        fv2 (map #(f (- c %)) absc)
        fv (map + fv1 fv2)
        abs-sum (map #(+ (abs %1) (abs %2)) fv1 fv2)]
    {:fv fv, :fv1 fv1, :fv2 fv2,
     :r10 (reduce + (map * w10 fv)),
     :r21 (reduce + (map * w21 fv)),
     :r-abs (reduce + (map * w21 abs-sum))}))
  
(defn comp-resasc [l1 l2 m]
  (let [sm (fn [v] (map #(- % m) v)) ;subst-mean
        sum (fn [{:keys [fv1 fv2]}] (map + (sm fv1) (sm fv2)))
        loop-term (fn [l w21] (map + w21 (sum l)))]
    (reduce + (map + (loop-term l1 w21a) (loop-term l2 w21b)))))
    
(defmethod formula 43 [nb-ev f hl center {:keys [r21] :as p}]
  (form-higher-order nb-ev f hl center p w43a w43b 11 :r43 r21))

(defmethod formula 87 [nb-ev f hl center {:keys [r43] :as p}]
  (form-higher-order nb-ev f hl center p w87a w87b 22 :r87 r43))

(defn form-higer-order [nb-ev f hl c p wxxa wxxb wxxb-index res-key resxx-inf]
  "helper function for 43 and 87 formulae"
  (let [rxx-init (* (wxxb wxxb-index) (f c))
        rxx (reduce + rxx-init (map * (p :savfun) wxxa))
        absc (map * (cycle hl) x4)
        op-f (fn [op]
                (map f (map #(op % c) absc)))
        fval (map + (op-f +) (op-f -))
        resxx (reduce rxx + (map + fval wxxb))
        err (rescale-err (* (- resxx resxx-inf) hl) p)]
    (assoc p
        :savfun (concat (p :savfun) fval)
        res-key resxx
        :err err
        :res (* resxx hl)
        :nb-ev nb-ev)))

;;; Generic helper fns

(defn rescale-err [err {:keys [r-abs r-asc]}]
  (do
  (prn "err=" err "r-abs=" r-abs "r-asc=" r-asc "epsilon=" (epsilon))
  (let [scale (fn [e]
                (if (every? #(not= % 0) [e r-asc])
                  (let [scale (pow (* 200 e (/ 1 r-asc)) 1.5)]
                    (if (< scale 1) (* r-asc scale) r-asc))
                  e))
        min-err (fn [e]
                  (if (> r-abs (/ min-pos-real (* 50 (epsilon))))
                    (let [min-err (* 50 (epsilon) r-abs)]
                      (if (> min-err e) min-err))
                    e))]
  (-> err (abs) (scale) (min-err)))))

(defn convergent? [abs-acc rel-acc {:keys [err res]}]
  (or (< err abs-acc)
      (< err (* rel-acc (abs res)))))

;;; adaptive

(defn kronrod-adaptive [acc max-ev]
  (if (< max-ev 15)
    (throw (IllegalArgumentException. "max-evals must be >= than 15"))
    (reify Integrator
      (integrate [_ f a b]
        (integ-recur f a b acc max-ev 0)))))

(defn integ-recur [f a b tol max-ev nb-ev]
  (let [hl (half (- b a))
        c (half (+ a b))
        ts (map #(* hl %) k15t)
        fsum-fn (fn [t] (+ (f (- c t)) (f (+ c t))))
        fsums (map fsum-fn ts)
        fsums-every-other (->> fsums (partition 2) (map first))
        ;indexing starts at 1 in C++ code
        g7 (->> (rest g7w)
                (map * (rest fsums-every-other))
                (reduce + (* (f c) (first g7w)))
                (* hl))
        k15 (->> (rest k15w)
                 (map * (rest fsums))
                 (reduce + (* (f c) (first k15w)))
                 (* hl))
        new-nb-ev (+ nb-ev 15)]
    (if (< (abs (- k15 g7)) tol)
      k15
      (if (> (+ nb-ev 30) max-ev)
        (throw (Exception. (str (+ 30 nb-ev) ">" max-ev" max nr of func evals exceeded")))
        (+ (integ-recur f a c (half tol) max-ev new-nb-ev)
           (integ-recur f c b (half tol) max-ev new-nb-ev))))))


;;; non-adaptive parameters

(def x1 [0.973906528517171720077964012084452,
          0.865063366688984510732096688423493,
          0.679409568299024406234327365114874,
          0.433395394129247190799265943165784,
          0.148874338981631210884826001129720])

(def w10 [0.066671344308688137593568809893332,
          0.149451349150580593145776339657697,
          0.219086362515982043995534934228163,
          0.269266719309996355091226921569469,
          0.295524224714752870173892994651338])

(def x2 [0.995657163025808080735527280689003,
          0.930157491355708226001207180059508,
          0.780817726586416897063717578345042,
          0.562757134668604683339000099272694,
          0.294392862701460198131126603103866])

(def w21a [0.032558162307964727478818972459390,
            0.075039674810919952767043140916190,
            0.109387158802297641899210590325805,
            0.134709217311473325928054001771707,
            0.147739104901338491374841515972068])

(def w21b [0.011694638867371874278064396062192,
            0.054755896574351996031381300244580,
            0.093125454583697605535065465083366,
            0.123491976262065851077958109831074,
            0.142775938577060080797094273138717,
            0.149445554002916905664936468389821])

(def x3 [0.999333360901932081394099323919911,
          0.987433402908088869795961478381209,
          0.954807934814266299257919200290473,
          0.900148695748328293625099494069092,
          0.825198314983114150847066732588520,
          0.732148388989304982612354848755461,
          0.622847970537725238641159120344323,
          0.499479574071056499952214885499755,
          0.364901661346580768043989548502644,
          0.222254919776601296498260928066212,
          0.074650617461383322043914435796506])

(def w43a [0.016296734289666564924281974617663,
            0.037522876120869501461613795898115,
            0.054694902058255442147212685465005,
            0.067355414609478086075553166302174,
            0.073870199632393953432140695251367,
            0.005768556059769796184184327908655,
            0.027371890593248842081276069289151,
            0.046560826910428830743339154433824,
            0.061744995201442564496240336030883,
            0.071387267268693397768559114425516])

(def w43b [0.001844477640212414100389106552965,
            0.010798689585891651740465406741293,
            0.021895363867795428102523123075149,
            0.032597463975345689443882222526137,
            0.042163137935191811847627924327955,
            0.050741939600184577780189020092084,
            0.058379395542619248375475369330206,
            0.064746404951445885544689259517511,
            0.069566197912356484528633315038405,
            0.072824441471833208150939535192842,
            0.074507751014175118273571813842889,
            0.074722147517403005594425168280423])

(def x4 [0.999902977262729234490529830591582,
          0.997989895986678745427496322365960,
          0.992175497860687222808523352251425,
          0.981358163572712773571916941623894,
          0.965057623858384619128284110607926,
          0.943167613133670596816416634507426,
          0.915806414685507209591826430720050,
          0.883221657771316501372117548744163,
          0.845710748462415666605902011504855,
          0.803557658035230982788739474980964,
          0.757005730685495558328942793432020,
          0.706273209787321819824094274740840,
          0.651589466501177922534422205016736,
          0.593223374057961088875273770349144,
          0.531493605970831932285268948562671,
          0.466763623042022844871966781659270,
          0.399424847859218804732101665817923,
          0.329874877106188288265053371824597,
          0.258503559202161551802280975429025,
          0.185695396568346652015917141167606,
          0.111842213179907468172398359241362,
          0.037352123394619870814998165437704])

(def w87a [0.008148377384149172900002878448190,
            0.018761438201562822243935059003794,
            0.027347451050052286161582829741283,
            0.033677707311637930046581056957588,
            0.036935099820427907614589586742499,
            0.002884872430211530501334156248695,
            0.013685946022712701888950035273128,
            0.023280413502888311123409291030404,
            0.030872497611713358675466394126442,
            0.035693633639418770719351355457044,
            0.000915283345202241360843392549948,
            0.005399280219300471367738743391053,
            0.010947679601118931134327826856808,
            0.016298731696787335262665703223280,
            0.021081568889203835112433060188190,
            0.025370969769253827243467999831710,
            0.029189697756475752501446154084920,
            0.032373202467202789685788194889595,
            0.034783098950365142750781997949596,
            0.036412220731351787562801163687577,
            0.037253875503047708539592001191226])

(def w87b [0.000274145563762072350016527092881,
            0.001807124155057942948341311753254,
            0.004096869282759164864458070683480,
            0.006758290051847378699816577897424,
            0.009549957672201646536053581325377,
            0.012329447652244853694626639963780,
            0.015010447346388952376697286041943,
            0.017548967986243191099665352925900,
            0.019938037786440888202278192730714,
            0.022194935961012286796332102959499,
            0.024339147126000805470360647041454,
            0.026374505414839207241503786552615,
            0.028286910788771200659968002987960,
            0.030052581128092695322521110347341,
            0.031646751371439929404586051078883,
            0.033050413419978503290785944862689,
            0.034255099704226061787082821046821,
            0.035262412660156681033782717998428,
            0.036076989622888701185500318003895,
            0.036698604498456094498018047441094,
            0.037120549269832576114119958413599,
            0.037334228751935040321235449094698,
            0.037361073762679023410321241766599])

;;; adaptive parameters

(def g7w [0.417959183673469,
          0.381830050505119,
          0.279705391489277,
          0.129484966168870])

(def k15w [0.209482141084728,
           0.204432940075298,
           0.190350578064785,
           0.169004726639267,
           0.140653259715525,
           0.104790010322250,
           0.063092092629979,
           0.022935322010529])
(def k15t [0.000000000000000,
           0.207784955007898,
           0.405845151377397,
           0.586087235467691,
           0.741531185599394,
           0.864864423359769,
           0.949107912342758,
           0.991455371120813])
