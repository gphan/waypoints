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
  ([node] (closest-waypoint node ignore))
  ([node visited] (let [[n p] node
                         visited (conj visited n)
                         not-visited? (complement #(contains? visited (first %)))]
                     (->> waypoints
                          (filter not-visited?)
                          (map (fn [[m v]] [m (waypoints->distance p v)]))
                          (sort-by last <)
                          first))))

(defn- find-neighboring-nodes
  ([node] (find-neighboring-nodes node 5000))
  ([node threshold] (let [[node-name node-values] node
                          ignore-nodes (conj ignore node-name)
                          not-visited? (complement #(contains? ignore-nodes (first %)))]
                      (->> waypoints
                           (filter not-visited?)
                           (map (fn [[n v]] [n (waypoints->distance node-values v)]))
                           (filter #(< (last %) threshold))
                           (map first)))))

(defn- score-path
  "Scores the route between node1 -> node2"
  [node1 node2]
  (let [w1 (last node1)
        w2 (last node2)
        dist (waypoints->distance w1 w2)
        ele1 (or (:elevation w1) (:elevation w2) 0)
        ele2 (or (:elevation w2) (:elevation w1) 0)
        delta-elev (- ele1 ele2)]
    (+ (* 0.5 (:points w2)) (* 0.25 dist) (* 0.25 delta-elev))))

(defn best-path [path]
  "Determines the best path from the last node in the path sequence using the score-path function"
  (let [last-node-name (last path)
        node (find waypoints last-node-name)
        ignore-nodes (into #{} path)
        not-visited? (complement (fn [[n v]] (contains? ignore-nodes n)))
        best (->> waypoints
                  (filter not-visited?)
                  (map (fn [d] [(first d) (score-path node d)]))
                  (sort-by last >)
                  first
                  first)]
    (if (nil? best)
      path
      (recur (conj path best)))))
