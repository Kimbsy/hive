(ns hive.display
  (:require [quil.core :as q]
            [hive.transform :as t]
            [hive.utils :as u]))

(defn add-vtx
  [{:keys [x y]} i]
  (let [angle-deg (- 30 (* 60 i))
        angle-rad (* angle-deg (/ Math/PI 180))]
    (q/vertex (+ x (* u/hex-size (q/cos angle-rad)))
              (+ y (* u/hex-size (q/sin angle-rad))))))

(defn get-color
  [hex]
  (cond
    (:selected hex)
    u/yellow

    (:highlighted hex)
    u/green

    :else
    (:color hex)))

(defn draw-hex
  [[q hex] r]
  (let [x q
        y r
        pos {:x (+ (* x u/hex-width) (* y u/hex-width 0.5))
             :y (* y (* u/hex-height 3/4))}]
    (q/fill (get-color hex))
    (q/begin-shape)
    (doall (map #(add-vtx pos %) (range 6)))
    (q/end-shape :close)))

(defn draw-row
  [[r row-hexes]]
  (doall (map #(draw-hex % r) row-hexes)))

(defn draw-line
  [line]
  (q/stroke-weight 4)
  (apply q/line (map t/axial-to-pixel line))
  (q/stroke-weight 1))

(defn draw-active-line
  [state]
  (q/stroke-weight 4)
  (apply q/line (map t/axial-to-pixel
                     [(first (:active-line state))
                      (t/axial-mouse-pos  :y-off -100)]))
  (q/stroke-weight 1))

(defn draw-lines
  [state]
  (doall (map draw-line (:lines state)))
  (when (:active-line state)
    (draw-active-line state)))

(defn draw-state [state]
  (q/background 240)
  (q/with-translation [0 100]
    (doall (map draw-row (:hexes state)))
    (case (:mode state)
      :line (draw-lines state)
      nil)))
