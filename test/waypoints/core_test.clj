(ns waypoints.core-test
  (:require [clojure.test :refer :all]
            [waypoints.core :refer :all]))

(deftest tag-name-test
  (testing "Filters out tags in sequence and converts tagname into keyword"
    (let [data [{:tag :t1}
                {:tag :t2}
                {:tag :t3}
                {:tag :t2}
                {:tag :t1}]]
      (is (= [{:tag :t1} {:tag :t1}]
             (tag-name "t1" data)))
      (is (= [{:tag :t2} {:tag :t2}]
             (tag-name :t2 data))))))

(deftest child-node-test
  (let [xml {:tag :head
             :content [{:tag :test}]}]
    (testing "Gets child nodes inside the content property of an XML tag"
      (is (= [{:tag :test}]
             (child-node "test" xml))))))


(deftest child-node-content-test
  (testing "Fetches first item of child node's contents"
    (let [xml {:tag :head
               :content [{:tag :child
                          :content ["Expected"]}]}]
      (is (= "Expected"
             (child-node-content "child" xml))))))

(deftest deg->radian-test
  (testing "Converts from degrees to radians"
    (let [deg 180.0
          rads 3.141593
          epsilon 0.00001]
      (is (<= (Math/abs (- (deg->radian deg) rads)) epsilon)))))

(deftest waypoints->distance-test
  (testing "Accurately calculates distance in meters using haversine formula"
    (let [w1 {:lat 38.898556 :long -77.037852}
          w2 {:lat 38.897147 :long -77.043934}]
      (is (< (Math/abs (- 549.0
                          (waypoints->distance w1 w2))) 1)))))

(deftest waypoints->elevation-gain-test
  (testing "Calculates elevation differences"
    (let [w1 {:elevation 500.0}
          w2 {:elevation 1000.0}]
      (is (= 500.0
             (waypoints->elevation-gain w1 w2)))))
  (testing "Returns 0 if elevation gain is less than zero"
    (let [w1 {:elevation 1000.0}
          w2 {:elevation 500.0}]
      (is (= 0
             (waypoints->elevation-gain w1 w2))))))

(deftest apply-to-nodes-test
  (testing "Runs function with first parameter and all in data set"
    (let [f vector
          n [:Node1 :1]
          s [[:Node2 :2] [:Node3 :3]]]
      (is (= {:Node2 [:1 :2] :Node3 [:1 :3]}
             (apply-to-nodes f n s))))))

(deftest merge-key-val-test
  (testing "Empty call returns empty map"
    (is (= {}
           (merge-key-val))))
  (testing "Associates a key and value to empty map"
    (is (= {:banana "phone"}
           (merge-key-val {} [:banana "phone"]))))
  (testing "Associates key and value to existing map"
    (is (= {:banana "phone" :existing "value"}
           (merge-key-val {:existing "value"} [:banana "phone"])))))


(deftest node->node-distance-test
  (testing "Looks up distance in waypoint-distances map"
    (with-redefs [waypoint-distances {:Node1 {:Node2 100}}]
      (is (= 100
             (node->node-distance :Node1 :Node2))))))

(deftest kml-structure->file-test
  (testing "Emits XML structure to file"
    (with-redefs [spit (fn [_ text] text)]
      (let [xml {:tag :test
                 :attrs {:t 100}
                 :content ["Hello"]}]
        (is (= "<?xml version='1.0' encoding='UTF-8'?>\n<test t='100'>\nHello\n</test>\n"
               (kml-structure->file "test" xml)))))))

(deftest path->coordinates-test
  (testing "Generates 3-tuple of long, lat, and 0 altitude."
    (let [path [:N1 :N2 :N3]
          waypoints {:N1 {:lat 10
                          :long 10}
                     :N2 {:lat 20
                          :long 20}
                     :N3 {:lat 30
                          :long 30}}]
      (is (= [[10 10 0.0] [20 20 0.0] [30 30 0.0]]
             (path->coordinates path waypoints))))))

(deftest path->distance-test
  (testing "Generates distance using node->node-distance function and waypoints-all"
    (let [path [:N1 :N2 :N3]]
      (with-redefs [node->node-distance (constantly 10)]
        (is (= 20
               (path->distance path)))))))

(deftest path->score-test
  (testing "Generates score by sum using waypoints-all map"
    (let [path [:N1 :N2 :N3]]
      (with-redefs [waypoints-all {:N1 {:points 100}
                                   :N2 {:points 150}
                                   :N3 {:points 50}}]
        (is (= 300
               (path->score path)))))))

(deftest highest-scoring-path-result-test
  (testing "Returns the highest scoring path result based on score"
    (let [result1 [[:Start :Finish] 1000 100 100]
          result2 [[:Start :Finish] 1000 100 200]]
      (is (= result2
             (highest-scoring-path-result result1 result2)))))
  (testing "Identity call should return a zero score path result"
    (is (= [[] 0 0 0]
           (highest-scoring-path-result)))))

(deftest depth-first-path-test
  (let [waypoint-names #{:Finish}]
    (testing "Base case if current distance is over maximum values"
      (with-redefs [max-distance 1000
                    path->distance (constantly 1001)
                    path->elevation-gain (constantly 100)
                    path->score (constantly 1)]
        (let [path [:N1 :N2]]
          (is (= [path 1001 100 0]
                 (depth-first-path path waypoint-names)))))
      (with-redefs [max-elevation 100
                    path->distance (constantly 1000)
                    path->elevation-gain (constantly 101)
                    path->score (constantly 1)]
        (let [path [:N1 :N2]]
          (is (= [path 1000 101 0]
                 (depth-first-path path waypoint-names))))))
    (testing "Path ends on :Finish, return the path with score and distance"
      (with-redefs [max-distance 1000
                    path->distance (constantly 999)
                    path->elevation-gain (constantly 100)
                    path->score (constantly 1000)]
        (let [path [:Start :1 :2 :Finish]]
          (is (= [path 999 100 1000]
                 (depth-first-path path waypoint-names))))))
    (testing "Recursively maps self over possible next waypoints"
      (with-redefs [max-distance 1000
                    path->distance (constantly 500)
                    path->elevation-gain (constantly 100)
                    path->score (constantly 1000)]
        (let [path [:Start :1 :2]]
          (is (= [[:Start :1 :2 :Finish] 500 100 1000]
                 (depth-first-path path waypoint-names))))))))

(deftest insert-new-point-test
  (with-redefs [path->score (constantly 500)
                path->distance (constantly 400)
                path->elevation-gain (constantly 100)]
    (testing "Creates new path and calculates path and score"
      (is (= [[:Start :1] [:Finish] 500 400 100]
             (insert-new-point [:Start] [:Finish] :1))))))

(deftest replace-next-point-test
  (with-redefs [path->score (constantly 500)
                path->distance (constantly 400)
                path->elevation-gain (constantly 100)]
    (testing "Creates new path with first right list replaced and calculates distance and score"
      (is (= [[:Start :1] [] 500 400 100]
             (replace-next-point [:Start] [:Finish] :1))))))

(deftest best-path-test
  (with-redefs [max-distance 10000
                max-elevation 10000]
    (let [better-path [[:S :1 :2] [:Finish] 9999 100 1000]
          bad-path [[:S :1 :Finish] [] 1000 100 0]
          invalid-path [[:S :1 :2] [:3] 1000 100 1100]
          too-long-path [[:S :1 :2 :3 :4] [:Finish] 10001 100 10001]
          too-high-path [[:S :1 :2 :3 :4] [:Finish] 10001 10001 10001]]
      (testing "Original path is worse than new path and is replaced"
        (is (= better-path
               (best-path bad-path better-path))))
      (testing "Original path is better and is not replaced"
        (is (= better-path
               (best-path better-path bad-path))))
      (testing "Invalid path is ignore if it doesn't have Finish"
        (is (= bad-path
               (best-path bad-path invalid-path))))
      (testing "path that is over max-distance is ignored"
        (is (= bad-path
               (best-path bad-path too-long-path))))
      (testing "Path that is over max-elevation is ignored"
        (is (= bad-path
               (best-path bad-path too-high-path)))))))

(deftest hill-climb-path-test
  (with-redefs [path->score (constantly 1000)
                path->distance (constantly 1000)
                insert-new-point (constantly [:Start :1])
                waypoints-all {:1 nil :2 nil :3 nil :4 nil}]
    (testing "Not complete path returns the same path"
      (is (= [[:Start] 1000 1000]
             (hill-climb-path [:Start]))))))
