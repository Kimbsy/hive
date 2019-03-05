(ns hive.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def dark-blue [34 87 173])
(def light-blue [91 146 234])
(def green [66 244 119])

(def hex-size 30)
(def hex-width (* hex-size (Math/sqrt 3)))
(def hex-height (* hex-size 2))

(defn generate-row
  [start end i]
  (let [color (if (odd? i) dark-blue light-blue)]
    (reduce (fn [results q]
              (conj results {q {:color color}}))
            {}
            (range start end))))

(defn hex-map
  [size]
  (let [width (inc (* size 2))]
    (reduce (fn [results i]
              (let [start (if (< i size)
                            (- size i)
                            0)
                    end   (if (< i size)
                            width
                            (+ width (- size i)))]
                (conj results {i (generate-row start end i)})))
            {}
            (range width))))

(defn setup []
  (q/frame-rate 60)
  {:hexes (hex-map 8)})

(defn clear-highlights
  [state]
  (update state :hexes
          #(clojure.walk/postwalk
            (fn [h]
              (if (:color h)
                (dissoc h :highlighted)
                h))
            %)))

(defn pixel-to-axial
  [x y]
  [(float (/ (- (* x (/ (Math/sqrt 3) 3)) (* 1/3 y)) hex-size))
   (float (/ (* 2/3 y) hex-size))])

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

(defn highlight-hover
  [state]
  (let [[q r] (axial-round (pixel-to-axial (q/mouse-x) (- (q/mouse-y) 100)))]
    (if (get-in state [:hexes r q])
      (assoc-in state [:hexes r q :highlighted] true)
      state)))

(defn update-state [state]
  (-> state
      clear-highlights
      highlight-hover))

(defn add-vtx
  [{:keys [x y]} i]
  (let [angle-deg (- 30 (* 60 i))
        angle-rad (* angle-deg (/ Math/PI 180))]
    (q/vertex (+ x (* hex-size (q/cos angle-rad)))
              (+ y (* hex-size (q/sin angle-rad))))))

(defn draw-hex
  [[q {:keys [color highlighted]}] r]
  (let [x q
        y r
        pos {:x (+ (* x hex-width) (* y hex-width 0.5))
             :y (+ 100 (* y (* hex-height 3/4)))}]
    (if highlighted
      (q/fill green)
      (q/fill color))
    (q/begin-shape)
    (doall (map #(add-vtx pos %) (range 6)))
    (q/end-shape :close)))

(defn draw-row
  [[r row-hexes]]
  (doall (map #(draw-hex % r) row-hexes)))

(defn draw-state [state]
  (q/background 240)

  (doall (map draw-row (:hexes state))))

(q/defsketch quiltest
  :title "Hex testing"
  :size [1200 900]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
