(ns clusters.clusterize
  (:gen-class))
(use 'alex-and-georges.debug-repl)

(defstruct Point :x :y :potential)
(def Ra 3)
(def Rb (* Ra 1.5))
(def alpha (/ 4 (Math/pow Ra 2)))
(def beta (/ 4 (Math/pow Rb 2)))

(def e_top 0.5)
(def e_bottom 0.15)

(def points
  (memoize (fn []
    (vec (for
      [point [[0 3] [1 5] [2 4] [3 3] [2 2] [2 1] [1 0] [5 5] [6 5] [7 6] [5 3] [7 3] [6 2] [6 1] [8 1]]]
      (struct Point (get point 0) (get point 1)))
    )
  ))
)


(defn square_distance
  "Square of euclidean distance."
  [point_1 point_2]
  (defn square_diff [axis] (Math/pow (- (get point_2 axis) (get point_1 axis)) 2))
  (+ (square_diff :x) (square_diff :y))
)

(defn point_to_point_potential
  "Get potential of point relatively to one point."
  [point_1, point_2, coefficient]
  (Math/pow Math/E (- (* coefficient (square_distance point_1 point_2))))
)

(defn point_to_multiple_points_potential
  "Get potential of point relatively to multiple points."
  [points, base_point]
  (reduce +
    (for [point points]
      (point_to_point_potential base_point point alpha)
    )
  )
)
(def split_by_max_potential
  (memoize (fn [points]
    "Split set of points to ((biggest_potential_point)(rest points))."
    (let [max_potential (apply max (map #(get % :potential) points))]
      ((juxt filter remove) #(= max_potential (get % :potential)) points)
    )
  ))
)
(def points_with_potentials
  (memoize (fn []
;to add another points, we need only to add parameters here in function points
    (for [base_point (points)]
      (struct Point
        (get base_point :x)
        (get base_point :y)
        (point_to_multiple_points_potential (points) base_point)
      )
    )
  ))
)

(def max_potential_point (memoize (fn []
  (first (first (split_by_max_potential (points_with_potentials))))
)))

(def max_potential (memoize (fn []
  (get (max_potential_point) :potential)
)))

(def rest_points (memoize (fn []
    (second (split_by_max_potential (points_with_potentials)))
)))

(def revised_potentials
  (memoize (fn []
    (sort-by :potential >
      (for [point (rest_points)]
        (struct Point
          (get point :x)
          (get point :y)
          (- (get point :potential)
             (* (get (max_potential_point) :potential)
                (point_to_point_potential (max_potential_point) point beta)
             )
          )
        )
      )
    )
  ))
)

(defn clusterize
  [revised_potentials, cluster_centers, el_index]
  (if
    (< el_index (count revised_potentials))
    (let [point (get (vec revised_potentials) el_index)]
      (if
        (> (get point :potential) (* e_top (max_potential)))
        (clusterize revised_potentials (conj cluster_centers point) (inc el_index))
        (if
          (< (get point :potential) (* e_bottom (max_potential)))
          cluster_centers
          (let [d_min (apply min (for [center cluster_centers] (square_distance point center)))]
            (if
              (>= (+ (/ d_min Ra) (/ (get point :potential) (max_potential))))
              (clusterize revised_potentials (conj cluster_centers point) (inc el_index))
              (clusterize revised_potentials cluster_centers (inc el_index))
            )
          )
        )
      )
    )
    cluster_centers
  )
)

(defn out_clusterize
  []
  (println (sort-by :potential > (revised_potentials)))
  (clusterize (revised_potentials) [(max_potential_point)] 0))
