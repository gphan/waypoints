(ns waypoints.core
  (:require [clojure.xml :as xml]
            [clojure.java.io :as io]
            [clojure.set]
            [clojure.core.reducers :as r])
  (:gen-class))

; The following code is just to parse the XML file

(def root (-> "2015.gpx" io/resource io/file xml/parse))

(defn tag-name [cname tags] (filter #(= (keyword cname) (:tag %)) tags))

(defn child-node [cname element] (tag-name cname (:content element)))

(defn child-node-content
  "Assumes there is only one child node of that name, fetches it's contents and then returns it"
  [cname element]
  (->> element (child-node cname) first :content first))

; waypoints-all contains all the waypoints in a clojure data structure
(def waypoints-all (into {} (for [w (child-node :wpt root)]
                              [(keyword (child-node-content :name w))
                               {:lat  (->> w :attrs :lat read-string)
                                :long (->> w :attrs :lon read-string)
                                :elevation  (read-string (or (child-node-content :ele w) "nil"))
                                :points  (read-string (first (re-seq #"[0-9]+" (child-node-content :desc w))))}])))

; Begin code to for graph and solution finding

(defn deg->radian [deg]
  (Math/toRadians deg))

(defn waypoints->distance
  "Calculates the distance between two waypoints using the haversine formula"
  [w1 w2]
  (let [R 6378100.0 ; Earth's radius in meters
        {lat1 :lat lon1 :long} w1
        {lat2 :lat lon2 :long} w2
        lat1-rad (deg->radian lat1)
        lat2-rad (deg->radian lat2)
        lon1-rad (deg->radian lon1)
        lon2-rad (deg->radian lon2)
        delta-lat (deg->radian (- lat2 lat1))
        delta-lon (deg->radian (- lon2 lon1))
        sin-half-delta-lat (Math/sin (/ delta-lat 2))
        sin-half-delta-lon (Math/sin (/ delta-lon 2))
        a (+ (* sin-half-delta-lat sin-half-delta-lat) (* (Math/cos lat1-rad) (Math/cos lat2-rad) sin-half-delta-lon sin-half-delta-lon))
        c (* 2 (Math/atan2 (Math/sqrt a) (Math/sqrt (- 1 a))))]
    (* R c)))

; We use this to reduce the search set for every function below
; Filter west and east of Start point
;(def western-waypoints (filter (fn [[k v]] (<= (:long v) (:long (:Start waypoints-all)))) waypoints-all))
;(def eastern-waypoints (filter (fn [[k v]] (>= (:long v) (:long (:Start waypoints-all)))) waypoints-all))

; Reduce problem set until we can apply heuristics
(def partitioned-waypoints (partition 10 5 (shuffle (seq (keys waypoints-all)))))

(defn merge-distances
  ([] {})
  ([distances [key distance]] (assoc distances key distance)))

(defn node-distance-to-all [node all]
  (r/reduce merge-distances
            (r/map (fn [n] [(first n) (waypoints->distance (last node) (last n))]) all)))

; Calculate distances from one node to any other at startup
(def waypoint-distances (r/reduce merge-distances
                                  (r/map (fn [a] [(first a) (node-distance-to-all a waypoints-all)]) waypoints-all)))

(defn node->node-distance [n1 n2]
  (get-in waypoint-distances [n1 n2]))

(defn kml-structure->file [f structure]
  (spit f (with-out-str (xml/emit structure))))

(defn path->coordinates [path waypoints]
  (map (fn [node-name] (let [n (node-name waypoints)]
                         [(:long n) (:lat n) 0.0])) path))

(defn path->distance [path]
  (reduce + (map (fn [a b] (node->node-distance a b)) path (rest path))))

(defn path->score [path]
  (reduce + (map (fn [n] (:points (n waypoints-all))) path)))

(defn path-results->kml-structure [path-results]
  (let [[path distance points] path-results]
    {:tag :kml :attrs {:xmlns "http://earth.google.com/kml/2.0"}
     :content [{:tag :Document
                :content [{:tag :name :content ["Geocache Route"]}
                          {:tag :Placemark
                           :content [{:tag :name :content ["WCG 2015"]}
                                     {:tag :description :content [(str "Points: " points)
                                                                  (str "Distance: " distance)]}
                                     {:tag :LineString
                                      :content [{:tag :coordinates
                                                 :content (map #(clojure.string/join "," %) (path->coordinates path waypoints-all))}]}]}]}]}))

; Meters per second
(def avg-hiking-speed 1.34112)
(def meters-per-hour (* avg-hiking-speed (* 60 60)))
(def max-time 4) ; Hours
(def max-distance (* max-time meters-per-hour))

(defn highest-scoring-path-result
  ([] [[] 0 0])
  ([result1 result2]
   (let [[p1 d1 s1] result1
         [p2 d2 s2] result2]
     (if (> s2 s1) result2 result1))))

(defn depth-first-path [current-path waypoint-names]
  (let [current-location (last current-path)
        current-distance (path->distance current-path)
        current-score (path->score current-path)
        available (disj waypoint-names current-location)]
    (cond
      (> current-distance max-distance) [current-path current-distance 0]
      (= :Finish current-location) [current-path current-distance current-score]
      :else (r/fold highest-scoring-path-result
                    (r/map #(depth-first-path (conj current-path %) available) available)))))

(defn depth-first [waypoints-list]
  (r/map #(depth-first-path [:Start] (into (into #{} %) [:Start :Finish])) waypoints-list))

(defn output-path-results [path-results]
  (let [filename (str "path-" (System/currentTimeMillis) ".kml")]
    (do
      (->> path-results path-results->kml-structure (kml-structure->file filename))
      filename)))

(defn insert-new-point [path-left path-right new-node]
  (let [path (concat path-left [new-node] path-right)
        score (path->score path)
        distance (path->distance path)]
    [(conj path-left new-node) path-right score distance]))

(defn replace-next-point [path-left path-right new-node]
  (let [rest-right (rest path-right)
        path (concat path-left [new-node] rest-right)
        score (path->score path)
        distance (path->distance path)]
  [(conj path-left new-node) rest-right score distance]))

(defn best-path
  ([] [[] [] 0 0])
  ([o n]
   (let [[oleft oright os od] o
         [nleft nright ns nd] n]
     (if (or (not= :Finish (last (concat nleft nright)))
             (nil? n)
             (> nd max-distance))
       o (if (> ns os)
           n
           (if (and (= ns os) (< nd od)) n o))))))

(defn hill-climb-path [path]
  (loop [left [(first path)]
         right (rest path)
         score (path->score path)
         distance (path->distance path)
         avail (clojure.set/difference (into #{} (keys waypoints-all)) (into #{} path))]
    (if (empty? right)
      [left distance score]
      (let [current [(conj left (first right)) (rest right) score distance]
            insertions (map (partial insert-new-point left right) avail)
            replacements (map (partial replace-next-point left right) avail)
            all (concat replacements insertions [current])
            [l r s d :as best-solution] (r/fold best-path all)]
        (do
          (recur l r s d (disj avail (last l))))))))

(defn hill-climb [path-result]
  "Does a hill-climb optimization on the path result until it cannot anymore"
  (loop [p path-result
         last-p nil]
    (if (= p last-p)
      p
      (recur (hill-climb-path (first p)) p))))

(defn best-of-random-hill-climbs []
  (->> partitioned-waypoints
       (depth-first)
       (r/map hill-climb)
       (r/fold highest-scoring-path-result)))

(defn -main [& args]
  (if (empty? args)
    (do
      (println (str "Running random-restart hill-climb search..."))
      (let [path-results (best-of-random-hill-climbs)]
        (println (str "Best hill-climbed path: " path-results))
        (output-path-results path-results)))
    (let [path (doall (map #(keyword %) args))]
        (println (str "Optimizing manual path: " (pr-str path)))
        (let [optimized (hill-climb [path 0 0])]
          (println (str "Optimized form: " optimized))
          (output-path-results optimized)))))
