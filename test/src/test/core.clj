(ns test.core
  (:gen-class))
;; I) Le clojure de base
(defn abs
  "retourne la valeur absolue de `x'"
  [x]
  (if (> x 0)
    x
    (* x -1)))

(defn mention
  "-----"
  [note]
  (cond
    (< note 10) :refuse
    (< note 12) :passable
    (< note 14) :assez-bien
    (< note 16) :bien
    :else :tres-bien))
;; Unproper recusrion in Clojure
(defn fact-poor
  "calcule `n'!"
  [n]
  (if (zero? n)
    1
    (* n (fact-poor (- n 1)))))

;; Decent recursion via dual declaration
(defn fact-ite
  "factorielle, methode iterative"
  ([n] (fact-ite n 1))
  ([n acc]
   (if (zero? n)
     acc
     (fact-ite (dec n) (* n acc)))))

;; Proper recursion, assisted by loop-recur
(defn fact-rec
  "-----"
  [n]
  (loop
      [k n
       acc 1]
    (if (zero? k)
      acc
      (recur (dec k) (* k acc)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, Trololo!"))

;; Rappel
;; collections :
;; - vecteurs (tableau fonctionnels) [e1 e2 .. en]
;; - maps (tableau associatifs) {k1 n1, k2 n2, .. , kn nn}
;; - reursion (boucle fonctionnels) sur les collections (loop ...(recur ...))

;;Itération sur les collections :
;; - `seq`; retourne une séquence sur la collection si non-vide, `nil` sinon
;; - `first`; retourne le premier elem de la sequence, `nil` sinon
;; - `rest`; retourne la séquence de tous les éléments sauf le premier
;;
;;Exemples :
;; - (= (seq [1 2 3]) (1 2 3))
;; - (= (seq []) nil)
;; - (= (seq {:a 1 :b 2 :c 3}) ([:b 2] [:a 1] [:c 3]))

(defn pairs
  [n]
  (loop
      [i 0
       res []]
    (if (< i n)
      (recur (inc i) (if (even? i) (conj res i) res))
      res)))

;; Ensembles (collections)
;; "comme en maths" :
;; - un ensemble fini d'élements
;; - tous distincts
;; - non-ordonnés
;; - ex : #{:a :b :c} OK
;; - #{:a :b :c :b} lève une exception (doublon de :b)
;; - on s'en sert surtout pour des tests d'appartenance :

(= (contains? #{:a :b :c} :c) true)
(= (contains? #{:a :b :c} :e) false)

;; - ajout dans un ensemble : `conj'
(= (conj #{:a :b :c} :d) #{:a :b :c :d})
(= (conj #{:a :b :c} :b) #{:a :b :c})

;; II) Les séquences (et la paresse...)
;; Une séquence est une abstraction d'un processus de génération séquentiel
;; de données (~ Iterators and Streams en Java).
;;Laziness => ne générer des données que si nécesaire
;;Le point de vue opposé :
;; - on génère les données entières à l'avance
;; =>t qu'on est STRICT
;; strict : loop-recur, reduce, etc.
;; lazy : lazy-seq, filter, map
;;
;; Générateurs programmés
;; Construction stricte avec `cons'
;; ex :
(= (cons 1 (cons 2 (cons 3 ()))) (range 1 4))
(defn intervalle
  [a b]
  (loop
      [i (dec b)
       s ()]
    (if (>= i a)
      (recur (dec i) (cons i s))
      s)))

(= (intervalle 1 4) (range 1 4))

;; Générateur paresseux avec lazy-seq :
;; Exemple : calcul des n premiers prime numbers
;; En strict:

(defn divisible-par?
  "retourne true si n est divisible par un des entiers"
  [n entiers]
  (loop
      [s entiers]
    (and (seq s) (or (= (mod n (first s))) 0))
         (recur (rest s))))

(= (divisible-par? 3 #{2 4 7}) false)


(defn premiers [n]
  (loop
      [i 2, res #{}]
    (if (< (count res) n)
      (recur
       (inc i)
       (if (divisible-par? i res) res (conj res i))
       res))))

;; En lazy
;; 1e etape : générer les entiers naturels

(defn naturels
  ([] (naturels 0))
  ([n] (lazy-seq (cons n (naturels (inc n))))))

(defn premiers-seq
  ([] (premiers-seq 2 #{}))
  ([n prs]
   (if (divisible-par? n prs)
     (premiers (inc n) prs)
     (lazy-seq (cons n (premiers (inc n) (conj prs n)))))))

;; map - filter, combinateurs de séquences :

(= (take 6 (map (fn [x] (* x x)) (naturels 1))) (1 4 9 16 25 36))

(defn my-map [f s]
  (if (seq s)
    (lazy-seq (cons (f (first s)) (my-map f (rest s))))
    ()))

;; Cours II : séquences paresseuses (suite et fin)

;; `lazy-seq' pour générer des séquences
;; `map', `filter' pour les combinaisons
;; réalisation de séquences lazy -> strict
;; ex : `take', `take-while', `reduce' (la plus générale)

;; * Principe de réalisation -> produire une valeur (stricte)
;; -> séquence réalisés (tous les éléments sont calculés) (plutôt pour des tests)
;; -> collections (vector, map, set, etc.)
;; -> un entier, un boolean, etc.
;;
;; * réalisation -> `loop recur', `take', `reduce'

(defn produit [s]
  (loop [s s,res 1]
    (if (seq s)
      (recur (rest s) (* res (first s)))
      res)))

(defn my-take [n s]
  (loop [n n, s s, res []]
    (if (and (seq s) (> n 0))
      (recur (dec n) (rest s) (conj res (first s)))
      res)))

;; La fonction `reduce` réalise/réduit une séquence s finie
;; avec une fonction f de deux arguments :
;; -> 1e arg : res le résultat accumulé
;; -> 2e arg : e l'élément courant dans la séquence
;; f doit retourner la nouvelle valeur de res
;; -> on a besoin d'une valeur initialle, init


;;Destructuration (de données)
;; pour le `let` et les paramètres de fonctions

(let [[p1,p2] seg
      x1 (first p1)
      y1 (second p1)
      x2 (first p2)
      y2 (first p2)])

(let [[a b c & rest] [1 2 3 4 5]]
  {:e1 a :e2 b :e3 c :r rest}) msb

(let [_ (print "bonjour")])

(let [{a :a, b :b, c :c} {:a 1 :b 2 :c 3}] (+ a b c))

;; Cours 3: Macros LISP - Méta-programmation
;; En LISP, le code source est une donnée manipulable (à la compilation)
;; Exemple :

(defn aire-triangle
  [a b c]
  (let [p (/ (+ a b c) 2)]
    (Math/sqrt (* p (- p a) (- p b) (- p c)))))
(= (aire-triangle 3 4 5) 6.0)

;; (conj () 4) -> (4)
;; (conj (conj () 4) 3) -> (3 4)

(= (list 1 2 3 4) '(1 2 3 4))
(= (list '+ 'a 'b 'c) '(+ a b c))

;; [e1 v1 ... en vn] -> vecteurs

;; Les citations
(= (quote toto) 'toto)

;; '<expr> construit l'expression complète
;; Les macros
;; exemple : la macro `when`

(defmacro when
  "------"
  [test & body]
  (list 'if test
        (cons 'do body)
        nil))

(defmacro if-not [expr then else]
  (list 'if (list 'clojure.core/not expr)
        then
        else))


