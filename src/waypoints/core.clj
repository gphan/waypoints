(ns waypoints.core
  (:require [clojure.xml :as xml]
            [clojure.java.io :as io]))

(def root (-> "2015.gpx" io/resource io/file xml/parse))

(defn- tag-name [cname tags] (filter #(= (keyword cname) (:tag %)) tags))

(defn- child-node [cname element] (tag-name cname (:content element)))

(defn- child-node-content
  "Assumes there is only one child node of that name, fetches it's contents and then returns it"
  [cname element]
  (->> element (child-node cname) first :content first))

(def waypoints (into {} (for [w (child-node :wpt root)]
                          [(keyword (child-node-content :name w))
                           {:lat  (->> w :attrs :lat read-string)
                            :long (->> w :attrs :lon read-string)
                            :elevation  (read-string (or (child-node-content :ele w) "nil"))
                            :points  (read-string (first (re-seq #"[0-9]+" (child-node-content :desc w))))}])))

(defn- deg->radian [deg]
  (Math/toRadians deg))

(defn- waypoints->distance
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

(defn- node-distance-to-all [node]
  (into {} (map (fn [n] [(first n) (waypoints->distance (last node) (last n))]) waypoints)))

(def waypoint-distances (into {}
                              (map (fn [a] [(first a) (node-distance-to-all a)]) waypoints)))

(defn- node->node-distance [n1 n2]
  (get-in waypoint-distances [n1 n2]))

(def start (find waypoints :Start))
(def finish (find waypoints :Finish))

(defn- output-path-file [structure]
  (spit "path.kml" (with-out-str (xml/emit structure))))

(defn- path->coordinates [path]
  (map (fn [node-name] (let [n (node-name waypoints)]
                         [(:long n) (:lat n) 0.0])) path))

(defn- path->distance [path]
  (reduce + (map (fn [a b] (node->node-distance a b)) path (rest path))))

(defn- path->score [path]
  (reduce + (map (fn [n] (:points (n waypoints))) path)))

(defn- coordinates->kml-structure [coords]
  {:tag :kml :attrs {:xmlns "http://earth.google.com/kml/2.0"}
   :content [{:tag :Document
              :content [{:tag :name :content ["Geocache Route"]}
                        {:tag :Placemark
                         :content [{:tag :name :content ["WCG 2015"]}
                                   {:tag :LineString
                                    :content [{:tag :coordinates
                                               :content (map #(clojure.string/join "," %) coords)}]}]}]}]})

; Meters per second
(def avg-hiking-speed 1.34112)
(def meters-per-hour (* avg-hiking-speed (* 60 60)))
(def max-time 4) ; Hours
(def max-distance (* max-time meters-per-hour))
(def available-waypoints (disj (set (keys waypoints)) :Start))

(defn- max-score [path-score]
  (reduce #(if (> (last %1) (last %2)) %1 %2) path-score))

(defn- find-path [current-path available]
  (let [current-location (last current-path)
        current-node-data (current-location waypoints)
        current-distance (path->distance current-path)]
    (cond
      (> current-distance max-distance) [current-path current-distance 0]
      (= :Finish current-location) [current-path current-distance (path->score current-path)]
      :else (max-score (map #(find-path (conj current-path %) (disj available %)) available)))))

(defn find-best-path []
  (find-path [:Start] (conj (set (take 20 (shuffle available-waypoints))) :Finish)))
