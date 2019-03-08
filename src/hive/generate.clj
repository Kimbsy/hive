(ns hive.generate
  (:require [hive.utils :as u]))

(defn generate-row
  [start end i]
  (reduce (fn [results q]
            (conj results {q {:color (if (odd? i)
                                       u/dark-blue
                                       u/light-blue)}}))
          {}
          (range start end)))

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
