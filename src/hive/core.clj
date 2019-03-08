(ns hive.core
  (:require [hive.display :as d]
            [hive.generate :as g]
            [hive.input :as i]
            [hive.transform :as t]
            [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  (q/frame-rate 60)
  {:mode :none
   :hexes (g/hex-map 8)})

(defn clear-highlights
  [state]
  (update state :hexes
          #(clojure.walk/postwalk
            (fn [h]
              (if (:color h)
                (dissoc h :highlighted)
                h))
            %)))

(defn highlight-hover
  [state]
  (let [state (clear-highlights state)
        [q r] (t/axial-mouse-pos :y-off -100)]
    (if (get-in state [:hexes r q])
      (assoc-in state [:hexes r q :highlighted] true)
      state)))

(defn highlight-under-active-line
  [state]
  state)

(defn mode-update-fn
  [state]
  (case (:mode state)
    :none highlight-hover
    :line (comp highlight-hover highlight-under-active-line)))

(defn update-state [state]
  (let [mode-fn (mode-update-fn state)]
    (-> state
        mode-fn)))

(q/defsketch quiltest
  :title "Hex testing"
  :size [1200 900]
  :setup setup
  :update update-state
  :draw d/draw-state
  :mouse-pressed i/mouse-pressed
  :key-pressed i/key-pressed
  ;; :features [:keep-on-top]
  :middleware [m/fun-mode])
