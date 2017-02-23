(ns thrones-bones.game-logic
  (:require [reagent.core :refer [atom]]
            [thrones-bones.util :refer [vec-join exists-in?]]))

;; Board building
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

(def board (vec (map #(vec (map template->square %))
                     board-template)))

;; Initial Pieces
(defn new-piece
  ([team x y] (new-piece team x y false))
  ([team x y leader]
   {:team team
    :coords [x y]
    :leader leader}))

(defn starting-pieces []
  (let [black
        (map #(apply (partial new-piece :black) %)
             [[0 4 true] ;; leader
              [3 0] [4 0] [5 0] [4 1]
              [0 3] [0 5] [1 4]
              [3 8] [4 8] [5 8] [4 7]
              [8 3] [8 4] [8 5] [7 4]])
        white
        (map #(apply (partial new-piece :white) %)
             [[4 4 true] ;; leader
              [4 2] [4 3] [2 4] [3 4]
              [5 4] [6 4] [4 5] [4 6]])
        all-pieces (vec-join black white)]
    all-pieces))

(defn new-game-state []
  {:pieces (starting-pieces)
   :turn :white
   :state :playing
   :history '()
   :replay '()
   })

(defonce page-state (atom {:bottom-section :game
                           :mute-music true
                           :mute-sound false}))

(defonce app-state (atom (new-game-state)))

(defn get-by-id [id]
  (.getElementById js/document id))

(def sounds
  {:fanfare (get-by-id "fanfare")
   :drum (get-by-id "drum")
   :piece-click (get-by-id "piece-click")
   :piece-select (get-by-id "piece-select")
   :music (get-by-id "ambient-music")})

(defn play-sound [key]
  (.play (key sounds)))

(defn stop-sound [key]
  (.pause (key sounds)))

(defn play-sfx [key]
  (if (not (:mute-sound @page-state))
    (play-sound key)))

()

;; Win Checking
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

(defn next-game-state [pieces]
  (or (check-white-win pieces)
      (check-black-win pieces)
      :playing))

(defn new-game! []
  (reset! app-state (new-game-state)))

(defn change-turn! []
  (swap! app-state assoc :turn (if (= (:turn @app-state) :white) :black :white)))

(defn finish-move-cleanup!
  ([] (finish-move-cleanup! false))
  ([replay-mode?]
   (let [next-state (next-game-state (:pieces @app-state))]
     (if (= :playing next-state)
       (change-turn!)
       (do
         (if replay-mode?
           (change-turn!)
           (play-sfx :drum))
         (swap! app-state assoc :state next-state))))))

(defn clear-redo! []
  (swap! app-state assoc :redo '()))

(defn push-redo! []
  (swap! app-state assoc :redo
         (conj (:redo @app-state) (:pieces @app-state))))

(defn push-undo! []
  (swap! app-state assoc :undo
         (conj (take 9 (:undo @app-state)) (:pieces @app-state))))

(defn pop-redo! []
  (if (not (empty? (:redo @app-state)))
    (do
      (push-undo!)
      (play-sfx :piece-click)
      (swap! app-state assoc :pieces (first (:redo @app-state)))
      (finish-move-cleanup! true)
      (swap! app-state assoc :redo (rest (:redo @app-state))))))

(defn pop-undo! []
  (if (not (empty? (:undo @app-state)))
    (do
      (push-redo!)
      (change-turn!)
      (play-sfx :piece-click)
      (swap! app-state assoc :state :playing)
      (swap! app-state assoc :pieces (first (:undo @app-state)))
      (swap! app-state assoc :undo (rest (:undo @app-state))))))

;; ===== BOARD/PIECE CHECKING =====

(defn is-the-piece-here? [piece x y]
  (let [[px py] (:coords piece)]
    (and (= x px) (= y py))))

(defn get-piece-at-pos [pieces x y]
  (filter #(is-the-piece-here? % x y)
          pieces))

(defn is-selecting-valid-piece? [selected turn piece]
  (and (not (nil? piece)) (= (:team piece) turn)))

(defn is-deselecting-piece? [selected piece]
  (and (not (nil? piece))
       (is-the-piece-here? piece (first selected) (second selected))))

;; ======= EVENT HANDLING ========
(defn get-next-coords [coords pos next-fn]
  (if (= pos first)
    [(next-fn (first coords)) (second coords)]
    [(first coords) (next-fn (second coords))]))

(defn reverse-direction [next-fn]
  (if (= next-fn inc) dec inc))

(defn get-default [coords]
  (let [[ox oy] coords
        x (if (>= ox 9) 8
              (if (< ox 0) 0
                  ox))
        y (if (>= oy 9) 8
              (if (< oy 0) 0
                  oy))]
    [x y]))

(defn my-search [board pieces predicate coords pos next-fn]
  "Given a board, a seq of pieces, a predicate function 
   for filtering squares, a vector tuple of coordinates, 
   a positional function (e.g. first or second) and a 
   function to find the next value when recurring (e.g. inc or dec),
   return the last valid square.

   Predicate function should take a board square and possible piece"
  (if (and (< (first coords) 9)
           (< (second coords) 9)
           (>= (first coords) 0)
           (>= (second coords) 0))
    (if (predicate (get-in board coords)
                   (get-piece-at-pos pieces (first coords) (second coords)))
      (recur board pieces predicate (get-next-coords coords pos next-fn)
             pos next-fn)
      (get-next-coords coords pos (reverse-direction next-fn))
      )
    (get-default coords)))

(defn get-white-piece-predicate [original-piece]
  (fn [terrain piece]
    (let [valid-terrain (not= terrain :mound)
          empty-square (empty? piece)]
      (and valid-terrain empty-square))))

(defn get-black-piece-predicate [original-piece]
  (fn [terrain piece]
    (let [is-leader (:leader original-piece)
          has-moved (:has-moved original-piece)
          valid-terrain (if has-moved
                          (= terrain :normal)
                          (not= terrain :castle))
          is-empty (empty? piece)]
      (if is-leader
        true
        (and valid-terrain is-empty)))))

(defn get-valid-moves-up [board pieces piece predicate]
  (let [coords (:coords piece)
        [x y] coords]
    (if (<= y 0)
      []
      (let [[mx my] (my-search board pieces predicate
                               (get-next-coords coords second dec) second dec)]
        (for [fx [x]
              fy (range my y)]
          [fx fy])))))

(defn get-valid-moves-down [board pieces piece predicate]
  (let [coords (:coords piece)
        [x y] coords]
    (if (>= y 8)
      []
      (let [[mx my] (my-search board pieces predicate
                               (get-next-coords coords second inc)
                               second inc)]
        (for [fx [x]
              fy (range (inc y) (inc my))]
          [fx fy])))))

(defn get-valid-moves-left [board pieces piece predicate]
  (let [coords (:coords piece)
        [x y] coords]
    (if (<= x 0)
      []
      (let [[mx my] (my-search board pieces predicate
                               (get-next-coords coords first dec)
                               first dec)]
        (for [fx (range mx x)
              fy [y]]
          [fx fy])))))

(defn get-valid-moves-right [board pieces piece predicate]
  (let [coords (:coords piece)
        [x y] coords]
    (if (>= x 8)
      []
      (let [[mx my] (my-search board pieces predicate
                               (get-next-coords coords first inc)
                               first inc)]
        (for [fx (range (inc x) (inc mx))
              fy [y]]
          [fx fy])))))

(defn get-valid-moves [board pieces piece]
  (let [team (:team piece)
        get-pred-fn (if (= team :white)
                      get-white-piece-predicate
                      get-black-piece-predicate)
        predicate (get-pred-fn piece)
        valid-up (get-valid-moves-up board pieces piece predicate)
        valid-down (get-valid-moves-down board pieces piece predicate)
        valid-left (get-valid-moves-left board pieces piece predicate)
        valid-right (get-valid-moves-right board pieces piece predicate)
        all-valid-moves
        (concat valid-up valid-down valid-left valid-right)]
    (if (or (not (= team :white))
            (not (:leader piece)))
      (remove #(= [4 4] %) all-valid-moves)
      all-valid-moves)))

(defn select-piece! [piece x y]
  (let [pieces (:pieces @app-state)]
    (swap! app-state assoc :selected [x y])
    (play-sfx :piece-select)
    (if (and (= (:team piece) :black) (:leader piece))
      (swap! app-state assoc :valid-moves
             (filter #(empty? (apply (partial get-piece-at-pos pieces)  %))
                     (for [x (range 9)
                           y (range 9)]
                       [x y])))
      (swap! app-state assoc :valid-moves
             (get-valid-moves board pieces piece)))))

(defn check-captures [pieces coords x-func y-func]
  (let [[x y] coords
        [tx ty] [(x-func x) (y-func y)]
        [ox oy] [(x-func tx) (y-func ty)]
        piece (first (get-piece-at-pos pieces x y))
        target-piece (first (get-piece-at-pos pieces tx ty))
        opposite-piece (first (get-piece-at-pos pieces ox oy))
        team (:team piece)]
    (if (and (not (nil? target-piece))
             (not= (:team target-piece) team)
             (not (and (:leader target-piece) (= (:team target-piece) :white)))
             (not (nil? opposite-piece))
             (= (:team opposite-piece) team))
      [tx ty]
      [])))

(defn white-leader-blocked? [board pieces coords x-func y-func]
  (let [[ox oy] coords
        x (x-func ox)
        y (y-func oy)
        terrain (get-in board [x y])
        piece (first (get-piece-at-pos pieces x y))]
    (or (not= terrain :normal)
        (and (not (nil? piece)) (= (:team piece) :black)))))

(defn white-leader-captured? [board pieces]
  (let [wl (filter-leader pieces :white)
        coords (:coords wl)
        blocked-up (white-leader-blocked? board pieces coords identity dec)
        blocked-down (white-leader-blocked? board pieces coords identity inc)
        blocked-left (white-leader-blocked? board pieces coords dec identity)
        blocked-right (white-leader-blocked? board pieces coords inc identity)]
    (if (and blocked-up blocked-down blocked-left blocked-right)
      coords)))

(defn do-captures! [coords]
  (let [pieces (:pieces @app-state)
        captures-up (check-captures pieces coords identity dec)
        captures-down (check-captures pieces coords identity inc)
        captures-left (check-captures pieces coords dec identity)
        captures-right (check-captures pieces coords inc identity)
        white-leader (white-leader-captured? board pieces) 
        all-captures
          (filter #(not (empty? %))
                  [captures-up captures-down captures-left captures-right white-leader])
        new-pieces
        (filter #(not (exists-in? all-captures (:coords %)))
                (:pieces @app-state))]
    (swap! app-state assoc :pieces new-pieces)))

(defn handle-post-move! [pieces end]
  (swap! app-state assoc :pieces pieces)
  (swap! app-state assoc :selected nil)
  (swap! app-state assoc :valid-moves [])
  (do-captures! end)
  (finish-move-cleanup!))

(defn move-piece! [start end]
  (let [pieces (:pieces @app-state)
        [sx sy] start
        [ex ey] end
        target-terrain (get-in board [ex ey])
        current-piece (first (get-piece-at-pos pieces sx sy))
        next-piece-pos (assoc current-piece :coords end)
        next-piece (if (and (= (:team next-piece-pos) :black)
                            (not= target-terrain :mound))
                     (assoc next-piece-pos :has-moved true)
                     next-piece-pos)]
    (do
      (push-undo!)
      (clear-redo!)
      (handle-post-move!
       (conj (filter #(not= (:coords %) start) pieces) next-piece)
       end)
      (play-sfx :piece-click))))

(defn handle-click-square! [x y]
  (if (= :playing (:state @app-state))
    (let [selected (:selected @app-state)
          valid-moves (:valid-moves @app-state)
          is-valid-move (exists-in? valid-moves [x y])]
      (if (and selected is-valid-move)
        (move-piece! selected [x y])))))

(defn handle-click-piece! [x y]
  (if (= :playing (:state @app-state))
    (let [turn (:turn @app-state)
          selected (:selected @app-state)
          pieces (:pieces @app-state)
          valid-moves (:valid-moves @app-state)
          filtered (get-piece-at-pos pieces x y)
          piece (if (empty? filtered) nil (first filtered))]
      (if (is-selecting-valid-piece? selected turn piece)
        (select-piece! piece x y))      
      (if (is-deselecting-piece? selected piece) ;; clicking selected piece deselects
        (do (swap! app-state assoc :selected nil)
            (swap! app-state assoc :valid-moves [])))
        )))
