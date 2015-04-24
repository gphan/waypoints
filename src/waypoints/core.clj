(ns waypoints.core
  (:require [clojure.xml :as xml]
            [clojure.java.io :as io]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zip-xml]))


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
                                  :ele  (read-string (or (child-node-content :ele w) "nil"))
                                  :pts  (read-string (first (re-seq #"[0-9]+" (child-node-content :desc w))))}])))

(defn- deg->radian [deg]
  (Math/toRadians deg))

(defn- distance
  "Calculates the distance between two waypoints using the haversine formula"
  [w1 w2]
  (let [R 6378100.0 ; Earth's radius in meters
        lat1 (:lat w1)
        lat2 (:lat w2)
        lon1 (:long w1)
        lon2 (:long w2)
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

(def start (find waypoints :Start))
(def finish (find waypoints :Finish))
(def ignore #{:Start :Finish})

(defn- closest-waypoint
  ([point] (closest-waypoint point ignore))
  ([point visited] (let [[n p] point
                         visited (conj visited n)]
                     (first (sort-by last <
                                     (map (fn [[m v]] [m (distance p v)])
                                          (filter (complement #(contains? visited (first %))) waypoints)))))))

(defn shortest-path
  ([node] (shortest-path node ignore []))
  ([node visited path] (let [closest (closest-waypoint node visited)
                             closest-key (first closest)
                             node-key (first node)]
                         (if (nil? closest)
                           (conj path :Finish)
                           (recur (find waypoints closest-key) (conj visited node-key) (conj path node-key))))))
