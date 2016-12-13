(ns tickets.core
  (:gen-class))

(require '[semantic-csv.core :refer :all]
         '[clojure-csv.core :as csv]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clj-fuzzy.metrics :refer :all])

(defn inverted-csv-hash [line-vec]
  (let [keyval (first line-vec)
        keynames (rest line-vec)]
    (into {} (map #(hash-map (keyword %) keyval) keynames))))

(def suffix-abbrev-data 
  (->> (slurp "/opt/data/tickets/c1_suffix_abbrevs")
       (str/split-lines)
       (map #(str/split % #"," ))
       (map inverted-csv-hash)
       (into {})))

(defn suffix-abbrev [suffix]
  (let [kw-suffix (keyword suffix)]
    (get suffix-abbrev-data kw-suffix)))

(def ticket-data (slurp-csv "/opt/data/tickets/testing_data.csv"))

(def address-data (slurp-csv "/opt/data/tickets/Chicago_Street_Names.csv"
                             :cast-fns {:start_address ->int :end_address ->int}))

(defn between [start end n] (and (<= start n) (>= end n)))

(def uniq-suffixes (let [uniq-suffixes (distinct (map #(:suffix %) address-data))] 
                     (filter #(not= " " %) uniq-suffixes)))
(def uniq-streets (distinct (map #(:street %) address-data)))
(def uniq-dirs (distinct (map #(:dir %) address-data)))

(defn closest-str-jaro 
  [streetname street-vec ceiling]
  (keep #(if (>= (jaro streetname %) ceiling) %) street-vec))

(defn closest-str-leven 
  [streetname street-vec ceiling]
  (keep #(if (<= (levenshtein streetname %) ceiling) %) street-vec))

(defn is-within-range 
  [addr-hash n]
  (let [start (addr-hash :start_address)
        end (addr-hash :end_address)]
   (between start end n)))

(defn keep-keyval 
  [addr-hash keyname keyval]
  (let [addr-hash-val (keyname addr-hash)]
    (= addr-hash-val "")))

(defn pull-street-number
  [addr-vec] 
  (try (Integer. (first addr-vec)) (catch Exception e) ) )

(defn pull-street-dir 
  ([addr-vec] (pull-street-dir uniq-dirs addr-vec))
  ([possible-dirs addr-vec] (apply str (keep #(if (= % (second addr-vec)) %) possible-dirs))))

(defn pull-street-suffix 
  ([addr-vec] (pull-street-suffix uniq-suffixes addr-vec))
   ([possible-suffixes addr-vec] 
    (let [suffix (suffix-abbrev (last addr-vec))]
          (keep #(if (= % suffix) %) 
                     possible-suffixes))))

(defn valid-street [possible-streets addr] 
  (let [addr-str (str/join " " addr)]
  (apply str (keep #(if (= % addr-str) %) possible-streets))))

(defn pull-street-name
  ([addr-vec] (pull-street-name uniq-streets addr-vec))
  ([possible-streets addr-vec] 
   (let [addr-full (rest (rest addr-vec))
         addr-butlast (butlast addr-full)]
     (filter not-empty (map #(valid-street possible-streets %) [addr-full addr-butlast]))))

(defn reduce-possibles 
  ([addr-hash] (reduce-possibles address-data addr-hash))
  ([possibles addr-hash]
  (->> possibles
       (keep #(if (is-within-range % (:num addr-hash)) %) )
       (keep #(if (= (:dir addr-hash) (:dir %)) %))
       (keep #(if (or (= (:suffix addr-hash) (:suffix %)) (= "" (:suffix addr-hash))) %))
       (keep #(if (or (= (:street addr-hash) (:street %)) (= "" (:street addr-hash))) %)))))

(defn map-pieces 
  [addr-vec]
  {:num (pull-street-number addr-vec) 
   :dir (pull-street-dir addr-vec) 
   :street (pull-street-name addr-vec) 
   :suffix (pull-street-suffix addr-vec)})

(defn map-fuzzy-pieces 
  [addr-vec addr-hash possibles]
)

(defn parse-ticket-address 
  [address] 
  (let [addr-vec (str/split address #" ")]
    (->> (str/split address #" ")
         (map-pieces))))

(defn parse-ticket-line 
  [line]
  (parse-ticket-address (line :address)))

(defn parse-tickets 
  ([] (parse-tickets ticket-data))
  ([unparsed-lines] 
   (map #(parse-ticket-line %) unparsed-lines)))

(defn -main [& args] (map prn (parse-tickets)))
