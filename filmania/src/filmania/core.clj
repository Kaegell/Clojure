(ns filmania.core
    (require [clojure.data.csv :as csv]
             [clojure.java.io :as io]))


(defn csv-seq
    "Retourne une séquence à partir d'un fichier CSV."
    [filename]
    (with-open [in-file (io/reader filename)]
        (doall
            (csv/read-csv in-file))))


(defn parse-movie
    "Construit un enregistrement de film depuis un entrée lue depuis CSV."
    [title-year genres]
    (let [r (re-seq #"(.+) \((\d\d\d\d)\)$" title-year)
          title (get (first r) 1)]
        (try
            (let [year (Integer/parseInt (get (first r) 2))]
                {:title title
                 :year year
                 :genres (set (filter #(not= % "(no genres listed)")
                                      (clojure.string/split genres #"\|")))})
            (catch Exception _ nil))))


(defn movie-map
    "Construit une map de films à partir d'un base en CSV."
    [csv]
    (reduce (fn [m [movie-id title-year genres]]
                (if-let [movie (parse-movie title-year genres)]
                    (assoc m (Integer/parseInt movie-id) movie)
                    m))
            {} csv))


(def movie-filename "resources/ml-latest-small/movies.csv")


(def movies (movie-map (rest (csv-seq movie-filename))))


(defn print-question1
    "Affiche les réponses de la question 1:
    - Combien y a-t-il de films dans la base ?
    - Combien y a-t-il de films de science-fiction ?
    - Combien y a-t-il de films de romance ?"
    []
    (let [s-csv (csv-seq movie-filename)]
        (do
            ;; Première question
            (println (str "Cette base contient " (count s-csv) " films."))

            ;; Deuxième question
            (let [nb-films-SF
                  (count (filter #(contains? (:genres (second %)) "Sci-Fi") movies))]
                (println (str "Il y a " nb-films-SF " films de science-fiction.")))

            ;; Troisième question
            (let [nb-films-romance
                  (count (filter #(contains? (:genres (second %)) "Romance") movies))]
                (println (str "Il y a " nb-films-romance " films de romance."))))))


(defn all-genres
    "Retourne un ensemble de tous les genres de films de la base `mov`."
    [mov]
    (loop
        [mov mov
         acc #{}]
        (if (seq mov)
            (recur (rest mov)
                   (loop
                       [genres (:genres (second (first mov)))
                        acc acc]
                       (if (seq genres)
                           (recur (rest genres) (conj acc (first genres)))
                           acc)))
            acc)))

(all-genres movies)
;; => #{"Children" "Fantasy" "War" "Sci-Fi" "Comedy" "Western" "Musical" "Documentary" "Mystery"
;;      "Thriller" "Horror" "IMAX" "Adventure" "Romance" "Crime" "Drama" "Film-Noir" "Action"
;;      "Animation"}


(defn films-by-genre
    "Retourne la base de films de genre `genre` extraite depuis la base `mov`."
    [mov genre]
    (filter #(contains? (:genres (second %)) genre) mov))

(count (films-by-genre movies "Sci-Fi"))
;; => 743

(count (films-by-genre movies "Romance"))
;; => 1564


(defn full-genres
    "Retourne une séquence contenant tous les genres (sans suppression de doublon) de la
    base `mov`."
    [mov]
    (loop
        [mov mov
         acc '()]
        (if (seq mov)
            (recur (rest mov)
                   (loop
                       [genres (:genres (second (first mov)))
                        acc acc]
                       (if (seq genres)
                           (recur (rest genres) (cons (first genres) acc))
                           acc)))
            acc)))

(defn card-genres
    "Retourne une map de cardinalité par genre depuis la base `mov`.
    Les clefs de cette map sont les genres, et les valeurs sont leur
    nombre d'apparitions dans `mov`."
    [mov]
    (frequencies (full-genres movies)))

(card-genres movies)
;; => {"Children" 588, "Fantasy" 609, "War" 402, "Sci-Fi" 743, "Comedy" 3258, "Western" 204,
;;     "Musical" 405, "Documentary" 436, "Mystery" 538, "Thriller" 1699, "Horror" 802,
;;     "IMAX" 143, "Adventure" 1041, "Romance" 1564, "Crime" 1080, "Drama" 4310,
;;     "Film-Noir" 111, "Action" 1433, "Animation" 389}

(first (apply max-key val (card-genres movies)))
;; => "Drama"

(first (apply min-key val (card-genres movies)))
;; => "Film-Noir"




;; ## Partie 2
(def ratings-filename "resources/ml-latest-small/ratings.csv")

;;TODO: Les ID des users sont sous forme de string, les caster en int
(defn parse-ratings
    "Parse la séquence contenue dans `ratings-seq` et retourne une map.
    Chaque clef de cette map correspond à l'identifiant d'un utilisateur, et
    chaque valeur contient une autre map representant chaque films associé à
    sa note donnée par cet utilisateur."
    [ratings-seq]
    ;; On itère sur la séquence des notes
    (loop [ratings-seq (rest ratings-seq), res {}]
        (if (seq ratings-seq)
            (let [id-user (first (first ratings-seq))
                  id-film (Integer/parseInt (second (first ratings-seq)))
                  stars (Double/parseDouble (nth (first ratings-seq) 2))
                  clock (last (first ratings-seq))
                  map-films-user (get res (first (first ratings-seq)))]
                (recur (rest ratings-seq) (assoc res id-user
                                              ;; On verifie si l'id de l'user est deja dans la map
                                              (if map-films-user
                                                  ;; On ajoute le film dans la map de cet user
                                                  (assoc map-films-user id-film stars)
                                                  ;; On ajoute l'user a la map de retour
                                                  (hash-map id-film stars)))))
            res)))

(def ratings (parse-ratings (csv-seq ratings-filename)))

(take 10 ratings)


(defn moyenne
    "Retourne la moyenne d'une séquence de nombres"
    [s]
    (if (seq s)
        (/ (reduce + s) (count s))
        0))

(moyenne [4 8 15])

(defn movie-avg-rating
    "Retourne une map associant à chaque film de la base `mov` sa note moyenne depuis
    la base `rat`."
    [mov rat]
    ;; On itère sur tous les films
    (loop [mov mov, res hash-map]
        (if (seq mov)
            (let [id-film (first mov)]
                (recur (rest mov)
                       (assoc res id-film (moyenne
                                              ;; On récupère une séquence des ratings de ce film
                                              ;; On itère sur chaque utilisateur
                                              (loop [rat rat, seq-ratings []]
                                                  (if (seq rat)
                                                      ;; On ajoute la note à la sequence de resultat si
                                                      ;; l'utilisateur a noté ce film
                                                      (recur (rest rat) (if (get (second (first rat)) id-film)
                                                                            (conj seq-ratings (get (second (first rat)) id-film))
                                                                            seq-ratings))
                                                      seq-ratings))))))
            res)))



(movie-avg-rating movies ratings)










;; Idées de statistiques :
;; - Chaque film le mieux noté et le moins bien noté dans chaque genre
;; - L'utilisateur ayant noté le plus de films
;; - Le genre de film le mieux noté (et le moins bien noté)
