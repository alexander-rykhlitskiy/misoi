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

(def import_points
  (memoize (fn [file_path]
    (for
      [point
        (for [str_point (str/split (slurp file_path) #"\n")]
          (vec (for [axis (str/split str_point #",")]
            (read-string axis))
          )
        )
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
(def part_by_max_potential
  (memoize (fn [points]
    "Split set of points to ((biggest_potential_point)(rest points))."
    (let [max_potential (apply max (map #(get % :potential) points))]
      ((juxt filter remove) #(= max_potential (get % :potential)) points)
    )
  ))
)
(def points_with_potentials
  (memoize (fn [points]
;to add another points, we need only to add parameters here in function points
    (for [base_point points]
      (struct Point
        (get base_point :coordinates)
        (point_to_multiple_points_potential points base_point)
      )
    )
  ))
)

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
  [points cluster_centers first_max_point]
  (let [parted_points (part_by_max_potential points)]
    (let [max_point (first (first parted_points)) rest_points (second parted_points)]
      (if
        (> (count points) 0)
        (if
          (> (get max_point :potential) (* e_top (get first_max_point :potential)))
          (recur
            (revised_potentials max_point rest_points)
            (conj cluster_centers max_point)
            first_max_point)
          (if
            (< (get max_point :potential) (* e_bottom (get first_max_point :potential)))
            cluster_centers
            (let [d_min (apply min (for [center cluster_centers] (Math/sqrt (square_distance max_point center 0))))]
              (if
                (>= (+ (/ d_min Ra) (/ (get max_point :potential) (get first_max_point :potential))))
                (recur (revised_potentials max_point rest_points) (conj cluster_centers max_point) first_max_point)
                (recur (revised_potentials max_point rest_points) cluster_centers first_max_point)
              )
            )
          )
        )
        cluster_centers
      )
    )
  )
)

(defn out_clusterize
  [file_path]
  (let [points (points_with_potentials (import_points file_path))]
    (let [parted_points (part_by_max_potential points)]
      (let [max_point (first (first parted_points)) rest_points (second parted_points)]
        (conj (clusterize rest_points () max_point) max_point)
      )
    )
  )
)
