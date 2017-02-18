(ns thrones-bones.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

;; CONFIG
(def white-color "#fafafa")
(def black-color "#444")
(def normal-color "#efeada")
(def mound-color "#908778")
(def castle-color "#bdb58e")
;; copied from:
;; https://clojuredocs.org/clojure.core/conj#example-56a6799ee4b060004fc217b0
(def vec-join (comp vec flatten conj))

(defn assoc-in-multiple [base & settings-pairs]
  (reduce (fn set-settings-pair [acc pair]
            (assoc-in acc [(first pair)] (second pair)))
          base
          settings-pairs))

;; define your app data so that it doesn't get over-written on reload

(def board-template
  [[0 0 0 1 1 1 0 0 0]
   [0 0 0 0 1 0 0 0 0]
   [0 0 0 0 0 0 0 0 0]
   [1 0 0 0 0 0 0 0 1]
   [1 1 0 0 2 0 0 1 1]
   [1 0 0 0 0 0 0 0 1]
   [0 0 0 0 0 0 0 0 0]
   [0 0 0 0 1 0 0 0 0]
   [0 0 0 1 1 1 0 0 0]])

(defn template->square [value]
  (case value
    0 :normal
    1 :mound
    2 :castle
    ))

(defn process-line [board-row]
  (vec (map template->square board-row)))

(defn new-board []
  (vec (map #(vec (map template->square %))
            board-template)))

(defn new-piece
  ([team x y] (new-piece team x y false))
  ([team x y leader]
   {:team team
    :coords [x y]
    :leader leader}))

(defn starting-pieces []
  (let [black
        (map #(apply (partial new-piece :black) %)
         [[3 0] [4 0] [5 0] [4 1]
          [0 3] [0 4] [0 5] [1 4]
          [3 8] [4 8] [5 8] [4 7]
          [8 3] [8 4] [8 5]])
        black-leader [(new-piece :black 7 4 true)]
        white
        (map #(apply (partial new-piece :white) %)
         [[4 2] [4 3] [2 4] [3 4]
          [5 4] [6 4] [4 5] [4 6]])
        white-leader [(new-piece :white 4 4 true)]
        ]
    (vec-join black black-leader white white-leader)))

(defn new-game-state []
   {:board (new-board)
    :pieces (starting-pieces)
    :turn :white})

(defonce app-state (atom (new-game-state)))

(defn filter-leader [pieces team]
  (first (filter #(and (= (:team %) team) (:leader %)) pieces)))

(defn check-white-win [pieces]
  (let [[x y] (:coords (filter-leader pieces :white))
        black-leader (filter-leader pieces :black)]
    (if (or (nil? black-leader) (= x 0) (= y 0) (= x 8) (= y 8))
      :white)))

(defn check-black-win [pieces]
  (if (nil? (filter-leader pieces :white))
    :black))

(prn "white win?" (check-white-win (:pieces @app-state)))
(prn "black win?" (check-black-win (:pieces @app-state)))

(defn game-over? [pieces]
  (let [white 5]))

(defn new-game! []
  (reset! app-state (new-game-state)))


(defn circle-conf [x y fill]
  {:r 0.3 :cx (+ x 0.45) :cy (+ y 0.45)
   :fill fill :stroke "#000" :stroke-width 0.015})

(defn rect-conf [x y fill]
  {:height 0.7 :width 0.7 :x (+ x 0.1) :y (+ y 0.1)
   :fill fill :stroke "#000" :stroke-width 0.015})

(defn render-piece [piece]
  (let [[x y] (:coords piece)
        [t c] (if (:leader piece)
                [:rect rect-conf]
                [:circle circle-conf])]
    (case (:team piece)
      :black [t (c x y black-color)]
      :white [t (c x y white-color)]
      nil)))

(defn render-pieces [pieces]
  (map render-piece pieces))

(defn render-square [x y color]
  [:rect {:width 0.9
          :height 0.9
          :fill color
          :x x
          :y y
          :stroke "#666"
          :stroke-width 0.015
          }])

(defn render-squares [board]
  (for [x (range 9)
        y (range 9)]
    (render-square
       x y
       (case (get-in board [x y])
         :mound mound-color
         :castle castle-color
         normal-color))))

(defn thrones-and-bones []
  [:div {:class "container"}
   [:center [:h1 "Thrones and Bones"]
    (into 
     (into
      [:svg {:view-box "0 0 9 9"
             :width "100%"
             :style {:margin-top 20
                     :max-width 500}}]
      (render-squares (:board @app-state)))
     (render-pieces (:pieces @app-state)))]   
])

(reagent/render-component [thrones-and-bones]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
