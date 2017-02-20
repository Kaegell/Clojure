(ns csp.part1)

;; CSP - Programmation Par Contraintes
;; Problèmes de satisfaction de contraintes :
;; - Variables, Domaines & Contraintes

;; Variables : représenter par des symboles (keyword plutôt que (quote))
;;
:x
:y
:z

;; Domaines : des ensembles discrets et bornés (finis)
;;
#{1 2 3 4 5}
#{:rouge :vert :bleu}

;; Relier les variables à leurs domaines?
;; => utiliser des maps

(def cercles-doms {:v1 (into #{} (range -1 15))
                   :v2 (into #{} (range -5 11))})

cercles-doms

;; Contraintes : limité aux contraintes binaires
;; On peut voir ça comme un prédicat :

(defn sq [x] (* x x))

(defn c1-check [x y]
  (<= (+ (sq (- x 9))
         (sq y))
      25))

(defn c2-check [x y]
  (<= (+ (sq (inc x))
         (sq (- y 2)))
      100))

(c1-check 6 2)
(c1-check 2 3)
(c2-check 6 2)
(c2-check 2 3)

;; Les contraintes identifient les variables
(defn mkvar [v i] (keyword (str v i)))
(def c1 {:var1 :v1
         :var2 :v2
         :check c1-check})

(def c2 {:var1 :v1
         :var2 :v2
         :check c2-check})

;; Exemple : coloriage de carte

(def coloriage-doms
  (into {} (map (fn [i]
                  [(keyword (str "v" i))
                   #{:bleu :rose :vert}])
                (range 1 8))))

(def coloriage-constraints
  (map (fn [x y] {:var1 (mkvar "v" x)
                  :var2 (mkvar "v" y)
                  :check not=})
       [1 1 2 2 3 3 3 4 5 6]
       [2 3 3 4 4 5 6 5 6 7]))

;; Exercice
;; TODO : representation du problème du zèbre en Clojure
