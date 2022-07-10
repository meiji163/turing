(ns turing.parser
  (:require [clojure.string :as str]) )

(defn parse-tm [rdr]
  (let [lines (str/split (slurp rdr) #"\n")
        [fst & rst] lines
        config (parse-config fst)
        tmap (apply hash-map (mapcat parse-transition rst))]
    (list config tmap)))

(defn parse-transition [line]
  (if-some [[whole state sym dir next write] 
            (map symbol 
                 (re-matches 
                  #"(\w+)\s(\w+)->(L|R)\s(\w+)\s(\w+)$" line))]
    (list 
     (list state sym)
     (list dir next write))
    (throw (Exception. (format "invalid transition: %s" line)))))

(def default-config 
  {:halt 'H
   :empty '_
   :sides 1
   :start-pos 0
   :input nil})

(defn parse-config [line]
  (let [config (clojure.edn/read-string line)]
    (merge default-config config)))
