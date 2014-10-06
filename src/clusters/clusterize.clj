(ns clusters.clusterize
  (:gen-class))
(use 'alex-and-georges.debug-repl)
(require '[clojure.string :as str])

(defstruct Point :coordinates :potential)
(def Ra 3)
(def Rb (* Ra 1.5))
(def alpha (/ 4 (Math/pow Ra 2)))
(def beta (/ 4 (Math/pow Rb 2)))

(def e_top 0.5)
(def e_bottom 0.15)

(def points
  (memoize (fn []
    (for
      [point
        (for [str_point (str/split (slurp "resources/бабочка.txt") #"\n")]
          (vec (for [axis (str/split str_point #",")]
            (Integer. (read-string axis)))
          ))
      ]
      (struct Point point))
  ))
)

(defn square_distance
  "Square of euclidean distance."
  [point_1 point_2 axis_number]
  (defn square_diff [axis_number]
    (Math/pow (- (get (get point_2 :coordinates) axis_number) (get (get point_1 :coordinates) axis_number)) 2))
  (if
    (< axis_number (count (get point_1 :coordinates)))
    (+ (square_diff axis_number) (square_distance point_1 point_2 (inc axis_number)))
    0
  )
)

(defn point_to_point_potential
  "Get potential of point relatively to one point."
  [point_1, point_2, coefficient]
  (Math/pow Math/E (- (* coefficient (square_distance point_1 point_2 0))))
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
        (get base_point :coordinates)
        (point_to_multiple_points_potential (points) base_point)
      )
    )
  ))
)

(def max_potential_point (memoize (fn [points]
  (first (first (split_by_max_potential points)))
)))

(def max_potential (memoize (fn [points]
  (get (max_potential_point points) :potential)
)))

(def rest_points (memoize (fn [points]
    (second (split_by_max_potential points))
)))

(def revised_potentials
  (memoize (fn
    [max_potential_point, rest_points]
    (sort-by :potential >
      (for [point rest_points]
        (struct Point
          (get point :coordinates)
          (- (get point :potential)
             (* (get max_potential_point :potential)
                (point_to_point_potential max_potential_point point beta)
             )
          )
        )
      )
    )
  ))
)

(defn clusterize
  [max_potential, revised_potentials, cluster_centers, el_index]
  (if
    (< el_index (count revised_potentials))
    (let [point (get (vec revised_potentials) el_index)]
      (if
        (> (get point :potential) (* e_top max_potential))
        (clusterize max_potential revised_potentials (conj cluster_centers point) (inc el_index))
        (if
          (< (get point :potential) (* e_bottom max_potential))
          cluster_centers
          (let [d_min (apply min (for [center cluster_centers] (square_distance point center 0)))]
            (if
              (>= (+ (/ d_min Ra) (/ (get point :potential) max_potential)))
              (clusterize max_potential revised_potentials (conj cluster_centers point) (inc el_index))
              (clusterize max_potential revised_potentials cluster_centers (inc el_index))
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
  (clusterize
    (max_potential (points_with_potentials))
    (revised_potentials
      (max_potential_point
        (points_with_potentials))
      (rest_points (points_with_potentials)))
    [(max_potential_point
        (points_with_potentials))]
    0
  )
)
