(ns hive.input
  (:require [quil.core :as q]
            [hive.transform :as t]))

(defn enable-line-mode
  [state]
  (prn "Line mode enabled")
  (-> state
      (assoc :mode :line
             :lines [])))

(defn disable-line-mode
  [state]
  (prn "Line mode disabled")
  (-> state
      (assoc :mode :none)
      (dissoc :lines
              :active-line)))

(defn toggle-line-mode
  [state]
  (if (= :line (:mode state))
    (disable-line-mode state)
    (enable-line-mode state)))

(defn key-pressed
  [state key-event]
  (case (:key key-event)
    :l (toggle-line-mode state)
    state))

(defn toggle-select-hex
  [state]
  (let [[q r] (t/axial-mouse-pos :y-off -100)]
    (if (get-in state [:hexes r q])
      (update-in state [:hexes r q :selected] not)
      state)))

(defn select-hex
  [state]
  (let [[q r] (t/axial-mouse-pos :y-off -100)]
    (if (get-in state [:hexes r q])
      (assoc-in state [:hexes r q :selected] true)
      state)))

(defn deselect-hex
  [state]
  (let [[q r] (t/axial-mouse-pos :y-off -100)]
    (if (get-in state [:hexes r q])
      (assoc-in state [:hexes r q :selected] false)
      state)))

(defn start-line
  [state]
  (-> state
      select-hex
      (update :active-line #(conj % (t/axial-mouse-pos :y-off -100)))))

(defn end-line
  [state mouse-event]
  (let [line (conj (:active-line state) (t/axial-mouse-pos :y-off -100))]
    (-> state
        select-hex
        (update :lines #(conj % line))
        (dissoc :active-line))))

(defn line-mode-click
  [state mouse-event]
  (let [[q r] (t/axial-mouse-pos :y-off -100)]
    (if (get-in state [:hexes q r])
      (if (:active-line state)
        (end-line state mouse-event)
        (start-line state))
      state)))

(defn mouse-pressed
  [state mouse-event]
  (case (:mode state)
    :none (toggle-select-hex state)
    :line (line-mode-click state mouse-event)))
