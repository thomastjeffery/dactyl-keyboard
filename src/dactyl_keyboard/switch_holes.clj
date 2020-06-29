(ns dactyl-keyboard.switch-holes
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.util :refer :all]
            [dactyl-keyboard.kailh-hole :refer :all]
            [dactyl-keyboard.sa-keycaps :refer :all]))

(def switch-hole kailh-hole)

(def mount-width (+ keyswitch-width 3))
(def mount-height (+ keyswitch-height 3))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def α (/ Math/PI 12))
(def β (/ Math/PI 36))

(def columns (range 0 6))
(def rows (range 0 4))

(def cap-top-height (+ plate-thickness sa-profile-key-height))
(def row-radius (+ (/ (/ (+ mount-height 1/2) 2)
                      (Math/sin (/ α 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width 2.0) 2)
                         (Math/sin (/ β 2)))
                      cap-top-height))
(def middle-finger-offset [0 2.82 -3.0])
(def pinky-offset [0 -5.8 5.64])

;; Places shape on column and row of a deformed plane.
(defn key-place [column row shape]
  (let [row-placed-shape (->> shape
                              (translate [0 0 (- row-radius)])
                              (rotate (* α (- 2 row)) [1 0 0])
                              (translate [0 0 row-radius]))
        column-offset (cond
                        (= column 2) middle-finger-offset
                        (>= column 4) pinky-offset
                        :else [0 0 0])
        column-angle (* β (- 2 column))
        placed-shape (->> row-placed-shape
                          (translate [0 0 (- column-radius)])
                          (rotate column-angle [0 1 0])
                          (translate [0 0 column-radius])
                          (translate column-offset))]
    (->> placed-shape
         (rotate (/ Math/PI 12) [0 1 0])
         (translate [0 0 13]))))

(defn place [shape]
  (apply union
         (for [column columns
               row rows
               :when (or (not= column 0) ;; Skip the bottom-left key to make room for the thumb cluster
                         (not= row 4))]
           (->> shape
                (key-place column row)))))

(def switch-holes (place switch-hole))
(def caps (place (sa-cap 1)))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness 3.5)
(def post-size 0.1)
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness)])))

(def post-adj (/ post-size 2))

;; A web-post for each corner of switch-hole
(def web-post-tr (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))

;; A web filling the space between each switch-hole
(def connectors
  (apply union
         (concat
          ;; Row connections
          (for [column (drop-last columns)
                row rows
                :when (or (not= column 0) ;; Skip the bottom-left key to make room for the thumb cluster
                          (not= row 4))]
            (triangle-hulls
             (key-place (inc column) row web-post-tl)
             (key-place column row web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place column row web-post-br)))

          ;; Column connections
          (for [column columns
                row (drop-last rows)
                :when (or (not= column 0) ;; Skip the bottom-left key to make room for the thumb cluster
                          (not= row 3))]
            (triangle-hulls
             (key-place column row web-post-bl)
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tl)
             (key-place column (inc row) web-post-tr)))

          ;; Diagonal connections
          (for [column (drop-last columns)
                row (drop-last rows)
                :when (or (not= column 0) ;; Skip the bottom-left key to make room for the thumb cluster
                          (not= row 3))]
            (triangle-hulls
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place (inc column) (inc row) web-post-tl))))))

;;;;;;;;;;;;;;
;; Edge Lip ;;
;;;;;;;;;;;;;;

(def edge-width 5)

;; A web surrounding swich-holes
;;; TODO: Factor out a macro
(def edge-lip
  (apply union
         (concat
          ;; Top-left corner
          (list
           (key-place 0 0
                      (triangle-hulls
                       (translate [(- edge-width) 0 0] web-post-tl)
                       (translate [0 edge-width 0] web-post-tl)
                       web-post-tl)))
          ;; Top row
          (for [column columns]
            (key-place column 0
                       (triangle-hulls
                        (translate [0 edge-width 0] web-post-tl)
                        (translate [0 edge-width 0] web-post-tr)
                        web-post-tl
                        web-post-tr)))
          ;; Top row filling
          (for [column (drop-last columns)]
            (triangle-hulls
             (key-place column 0 (translate [0 edge-width 0] web-post-tr))
             (key-place (inc column) 0 (translate [0 edge-width 0] web-post-tl))
             (key-place column 0 web-post-tr)
             (key-place (inc column) 0 web-post-tl)))
          ;; Top-right corner
          (list
           (key-place (last columns) 0
                     (triangle-hulls
                      (translate [0 edge-width 0] web-post-tr)
                      (translate [edge-width 0 0] web-post-tr)
                      web-post-tr)))
          ;; Right column
          (for [row rows]
            (key-place (last columns) row
                       (triangle-hulls
                        web-post-tr
                        (translate [edge-width 0 0] web-post-tr)
                        web-post-br
                        (translate [edge-width 0 0] web-post-br))))
          ;; Right column filling
          (for [row (drop-last rows)]
            (triangle-hulls
             (key-place (last columns) row (translate [edge-width 0 0] web-post-br))
             (key-place (last columns) (inc row) (translate [edge-width 0 0] web-post-tr))
             (key-place (last columns) row web-post-br)
             (key-place (last columns) (inc row) web-post-tr)))
          ;; Bottom-right corner
          (list
           (key-place (last columns) (last rows)
                      (triangle-hulls
                       (translate [edge-width 0 0] web-post-br)
                       (translate [0 (- edge-width) 0] web-post-br)
                       web-post-br)))
          ;; Bottom row
          (for [column columns]
            (key-place column (last rows)
                       (triangle-hulls
                        (translate [0 (- edge-width) 0] web-post-bl)
                        (translate [0 (- edge-width) 0] web-post-br)
                        web-post-bl
                        web-post-br)))
          ;; Bottom row filling
          (for [column (drop-last columns)]
            (triangle-hulls
             (key-place column (last rows) (translate [0 (- edge-width) 0] web-post-br))
             (key-place (inc column) (last rows) (translate [0 (- edge-width) 0] web-post-bl))
             (key-place column (last rows) web-post-br)
             (key-place (inc column) (last rows) web-post-bl)))
          ;; Bottom-left corner
          (list
           (key-place 0 (last rows)
                      (triangle-hulls
                       (translate [(- edge-width) 0 0] web-post-bl)
                       web-post-bl
                       (translate [0 (- edge-width) 0] web-post-bl))))
          ;; Left column
          (for [row rows]
            (key-place 0 row
                       (triangle-hulls
                        web-post-tl
                        (translate [(- edge-width) 0 0] web-post-tl)
                        web-post-bl
                        (translate [(- edge-width) 0 0] web-post-bl))))
          ;; Left column filling
          (for [row (drop-last rows)]
            (triangle-hulls
             (key-place 0 row (translate [(- edge-width) 0 0] web-post-bl))
             (key-place 0 (inc row) (translate [(- edge-width) 0 0] web-post-tl))
             (key-place 0 row web-post-bl)
             (key-place 0 (inc row) web-post-tl)))
          )))

(spit "things/switch-holes.scad"
      (write-scad (union
                   switch-holes
                   connectors
                   edge-lip)))
