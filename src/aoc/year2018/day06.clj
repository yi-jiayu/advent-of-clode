(ns aoc.year2018.day06)

(defn parse-coord
  [coord]
  (map #(Integer/parseInt %) (clojure.string/split coord #", ")))

(defn parse-coords
  [input]
  (->> input
       (clojure.string/trim)
       (clojure.string/split-lines)
       (map parse-coord)))

(defn get-bounding-box
  [coords]
  (let [xs (map first coords)
        ys (map second coords)
        min-x (apply min xs)
        max-x (apply max xs)
        min-y (apply min ys)
        max-y (apply max ys)]
    [[min-x min-y] [max-x max-y]]))

(defn points-in-grid
  [[min-x min-y] [max-x max-y]]
  (for [x (range min-x (inc max-x))
        y (range min-y (inc max-y))]
    [x y]))

(defn manhattan-distance
  [[x1 y1] [x2 y2]]
  (+ (Math/abs ^int (- x1 x2)) (Math/abs ^int (- y1 y2))))

(defn index-of-nearest-centroid
  [coord centroids]
  (let [distances (map (partial manhattan-distance coord) centroids)
        min-distance (apply min distances)]
    (if (< 1 (count (filter (partial = min-distance) distances)))
      -1
      (.indexOf distances min-distance))))

(defn cluster
  [coords centroids]
  (loop [clusters (into [] (repeat (count centroids) #{}))
         coords coords]
    (if (empty? coords)
      clusters
      (let [coord (first coords)
            nearest-centroid-index (index-of-nearest-centroid coord centroids)
            clusters (if (not= -1 nearest-centroid-index)
                       (update clusters
                               nearest-centroid-index
                               #(conj % coord))
                       clusters)]
        (recur clusters
               (rest coords))))))

(defn touching-boundary?
  [[min-x min-y] [max-x max-y] [x y]]
  (or (= x min-x)
      (= x max-x)
      (= y min-y)
      (= y max-y)))

(defn remove-infinite-clusters
  [top-left bottom-right clusters]
  (remove #(some (partial touching-boundary? top-left bottom-right) %)
          clusters))

(defn largest-area
  [coords]
  (let [[top-left bottom-right] (get-bounding-box coords)
        grid (points-in-grid top-left bottom-right)
        clusters (remove-infinite-clusters
                   top-left
                   bottom-right
                   (cluster grid coords))]
    (apply max 0 (map count clusters))))

(defn within-distance-from?
  [centroids distance coord]
  (let [distances (map (partial manhattan-distance coord) centroids)
        total-distance (apply + distances)]
    (> distance total-distance)))

(defn safest-region-size
  [threshold coords]
  (let [[top-left bottom-right] (get-bounding-box coords)
        grid (points-in-grid top-left bottom-right)]
    (count (filter (partial within-distance-from? coords threshold) grid))))
