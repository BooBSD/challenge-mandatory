(ns challenge-mandatory.core
  (:require [clojure.string :as str])
  (:import  [java.net URL])
  (:gen-class))


(defn- parse-token [^String conf ^String token]
  (map last (re-seq (re-pattern (str token "\\(([\\w\\.\\?\\/\\=]+)\\)")), conf)))

(defn- parse-host [^String conf]
  (first (parse-token conf "host")))

(defn- parse-path [^String conf]
  (if-let [path (first (parse-token conf "path"))]
    (into {} (vec (map-indexed #(if (str/starts-with? %2 "?")
                                  {%1 (keyword (str/replace %2 #"^\?" ""))}
                                  {%1 %2})
                               (str/split path #"/"))))))

(defn- parse-queryparam [^String conf]
  (if-let [queryparam (not-empty (parse-token conf "queryparam"))]
    (into {} (map (fn [qp]
                      (let [[n kw] (str/split qp #"=\?")]
                          [n (keyword kw)])) queryparam))))

(defn- recognize-host [host url]
  (= host (.getHost url)))

(defn- recognize-path [pattern url]
  (let [parts (-> url
                  .getPath
                  (str/replace #"^/" "")
                  (str/split #"/"))
        check-part (fn [[idx acc] value]
                     (let [part (pattern idx)]
                       (if (keyword? part)
                           [(inc idx) (conj acc [part value])]
                           (if (= part value)
                               [(inc idx) acc]
                               (reduced [0 nil])))))
        res (second (reduce check-part [0 []] parts))]
      (if (and (vector? res) (empty? res)) true res)))

(defn- recognize-queryparam [pattern url]
  (let [parts (-> url
                  .getQuery
                  (str/split #"&"))
        pairs (into {} (map #(str/split % #"=") parts))
        check-part (fn [acc [n kw]]
                     (if-let [value (pairs n)]
                         (conj acc [kw value])
                         (reduced nil)))]
;      (prn pairs)
;      (prn pattern)
      (reduce check-part [] pattern)))

(defn- new-config [^String conf]
  (filter val {:host       (parse-host conf)
               :path       (parse-path conf)
               :queryparam (parse-queryparam conf)}))

(defn new-pattern [^String conf]
  (let [dict {:host       recognize-host
              :path       recognize-path
              :queryparam recognize-queryparam}
        config (new-config conf)]
;    (prn config)
    (into {} (map (fn [[k v]] [k (partial (dict k) v)]) config))))

(defn recognize [pattern url]
  (let [url (URL. url)
        patterns (keep #(pattern %) [:host :path :queryparam])
        results (map #(% url) patterns)
        res (reduce #(if-not %2 (reduced nil) (if (vector? %2) (conj %1 %2))) [] results)]
      (if (seq? res) (vec (apply concat res)) res)))


(defn -main
  [& args]

  (def twitter (new-pattern "host(twitter.com); path(?user/status/?id);"))
  (def dribbble (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);"))
  (def dribbble2 (new-pattern "host(dribbble.com); queryparam(offset=?offset); queryparam(list=?type);"))

  (prn (recognize twitter "http://twitter.com/bradfitz/status/562360748727611392"))
  (prn (recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"))
  (prn (recognize dribbble "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"))
  (prn (recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users"))
  (prn (recognize dribbble2 "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"))
  (prn (recognize dribbble2 "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"))
  (prn (recognize dribbble2 "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users")))
