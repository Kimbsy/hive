(ns hive.transform
  (:require [hive.utils :as u]
            [quil.core :as q]))

(defn pixel-to-axial
  [[x y]]
  [(float (/ (- (* x (/ (Math/sqrt 3) 3)) (* 1/3 y)) u/hex-size))
   (float (/ (* 2/3 y) u/hex-size))])

(defn axial-to-pixel
  [[q r]]
  [(int (* u/hex-size (+ (* (Math/sqrt 3) q) (* (/ (Math/sqrt 3) 2) r))))
   (int (* u/hex-size 3/2 r))])

(defn axial-to-cube
  [[q r]]
  [q r (- 0 q r)])

(defn cube-to-axial
  [[x y z]]
  [x y])

(defn cube-round
  [[x y z]]
  (let [[rx ry rz] (map #(Math/round %) [x y z])
        [dx dy dz] (map (fn [a b] (Math/abs (- a b))) [x y z] [rx ry rz])]
    (cond
      (and (> dx dy) (> dx dz))
      [(- 0 ry rz) ry rz]

      (> dy dz)
      [rx (- 0 rx rz) rz]

      :else
      [rx ry (- 0 rx ry)])))

(defn axial-round
  [axial]
  (-> axial
      axial-to-cube
      cube-round
      cube-to-axial))

(defn axial-mouse-pos
  [& {:keys [x-off y-off] :or {x-off 0 y-off 0}}]
  (axial-round (pixel-to-axial [(+ (q/mouse-x) x-off)
                                (+ (q/mouse-y) y-off)])))
