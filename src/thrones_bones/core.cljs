(ns thrones-bones.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

;; CONFIG
(def white-color "#fafafa")
(def black-color "#444")
(def terrain-color {:normal "#efeada"
                    :mound "#908778"
                    :castle "#bdb58e"})
(def selected-color {:normal "#efeafa"
                     :mound  "#9087ab"
                     :castle "#bdb5af"})
(def game-over-color {:normal "#cdc8b8"
                      :mound "#7a756b"
                      :castle "#a8a288"})

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

(defonce page-state (atom {:bottom-section :game
                             :mute-music true
                             :mute-sound false}))
(defn play-sfx [key]
  (if (not (:mute-sound @page-state))
    (play-sound key)))

;; copied from:
;; https://clojuredocs.org/clojure.core/conj#example-56a6799ee4b060004fc217b0
(def vec-join (comp vec flatten conj))

(defn assoc-in-multiple [base & settings-pairs]
  (reduce (fn set-settings-pair [acc pair]
            (assoc-in acc [(first pair)] (second pair)))
          base
          settings-pairs))

(defn exists-in? [seq item]
  (if (or (nil? seq) (nil? item))
    false
    (contains? (zipmap seq (repeat true)) item)))

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

(defn new-board []
  (vec (map #(vec (map template->square %))
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
   {:board (new-board)
    :pieces (starting-pieces)
    :turn :white
    :state :playing})

(defonce app-state (atom (new-game-state)))


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

;; Test get-valid-moves
#_(let [board (:board @app-state)
      pieces (:pieces @app-state)
      piece (first (get-piece-at-pos pieces 4 4))]
  (prn "all valid moves for" (:coords piece))
  (prn (get-valid-moves board pieces piece)))

(defn select-piece! [piece x y]
  (let [board (:board @app-state)
        pieces (:pieces @app-state)]
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
  (let [board (:board @app-state)
        pieces (:pieces @app-state)
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
  (let [next-state (next-game-state (:pieces @app-state))]
    (if (= :playing next-state)
      (change-turn!)
      (do
        (swap! app-state assoc :state next-state)
        (play-sfx :drum)))))

(defn move-piece! [start end]
  (let [pieces (:pieces @app-state)
        [sx sy] start
        [ex ey] end
        target-terrain (get-in (:board @app-state) [ex ey])
        current-piece (first (get-piece-at-pos pieces sx sy))
        next-piece-pos (assoc current-piece :coords end)
        next-piece (if (and (= (:team next-piece-pos) :black)
                            (not= target-terrain :mound))
                     (assoc next-piece-pos :has-moved true)
                     next-piece-pos)]
    (do (handle-post-move!
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



;; ========== RENDERING ==========
;; Piece Rendering
(defn circle-conf [x y fill selected]
  {:r 0.3 :cx (+ x 0.45) :cy (+ y 0.45)
   :fill fill :stroke (if selected "#f00"  "#000")
   :stroke-width 0.015
   :on-click #(handle-click-piece! (- x 1) (- y 2))})

(defn rect-conf [x y fill selected]
  {:height 0.7 :width 0.7 :x (+ x 0.1) :y (+ y 0.1)
   :fill fill :stroke (if selected "#f00" "#000")
   :stroke-width 0.015
   :on-click #(handle-click-piece! (- x 1) (- y 2))})

(defn render-piece [selected piece]
  (let [[ox oy] (:coords piece)
        [t c] (if (:leader piece)
                [:rect rect-conf]
                [:circle circle-conf])
        y (+ oy 2)
        x (+ ox 1)
        selected (is-the-piece-here? piece (first selected) (second selected))]
    (case (:team piece)
      :black [t (c x y black-color selected)]
      :white [t (c x y white-color selected)]
      nil)))

(defn render-pieces [pieces selected]
  (map (partial render-piece selected) pieces))

;; Board Rendering
(defn render-square [x y color stroke]
  [:rect {:width 0.9
          :height 0.9
          :fill color
          :x x
          :y y
          :stroke stroke
          :stroke-width 0.015
          :on-click #(handle-click-square! (- x 1) (- y 2))
          }])

(defn render-squares [board valid-moves state]
  (for [x (range 9)
        y (range 9)]
    (let [is-valid-move (exists-in? valid-moves [x y])
          terrain (get-in board [x y])
          color-set (if (not= state :playing)
                      game-over-color
                      (if is-valid-move selected-color terrain-color))
          color (terrain color-set)]
      (render-square (+ x 1) (+  y 2) color
       (if (exists-in? valid-moves [x y])
         "#66a"
         "#666")))))

(defn render-turn-indicator-circle [color state]
  (render-piece [nil nil] {:coords [(if (= color :white) 2 6) -2]
                    :team color
                    :leader false}))

(defn render-turn-indicator-text [turn state]
  [:text {:x 4 :y 0.6
          :text-length 3
          :font-size 0.5}
   (str (if (= turn :white) "White" "Black")
        " "
        (if (= state :playing) "Turn" "WINS!"))])

(defn render-turn-indicator [turn state]
  [(render-turn-indicator-circle turn state)
   (render-turn-indicator-text turn state)])

(defn row-labels [x]
  (map (fn [y] [:text {:x x :y (+ y 2.6) :font-size 0.5} (str (inc y))])
       (range 9)))

(defn column-labels [y]
  [:text {:x 1.2 :y y :text-length 9 :font-size 0.5} "ABCDEFGHI"])

(defn render-labels []
  (concat (row-labels 10.1)
          (row-labels 0.6)
   [(column-labels 1.8)] [(column-labels 11.4)]))

(defn select-bottom-content! [key]
  (swap! page-state assoc :bottom-section key))

(defn bottom-link
  ([href content-key label current] (bottom-link href content-key label current true))
  ([href content-key label current shouldPreventDefault]
   [:span {:class "bottom-link-container"
           }
    [:a {:href href :class (if (= content-key current) "active" "")
         :on-click #(do (if shouldPreventDefault (.preventDefault %))
                        (select-bottom-content! content-key))} label]]))

(defn bottom-content [key current el]
  (if (= key current)
    el))

(defn toggle-music! []
  (swap! page-state assoc :mute-music (not (:mute-music @page-state)))
  (if (:mute-music @page-state)
    (stop-sound :music)
    (play-sound :music)))

(defn toggle-sound! []
  (swap! page-state assoc :mute-sound (not (:mute-sound @page-state))))

;; App Component
(defn thrones-and-bones []
  [:div {:class "container"}
   [:center [:h1 "Thrones and Bones"]
    [:div {:style {:margin-top 25 :margin-bottom 20}}
     [:a {:href "#"
          :class (str "button" (if (:mute-sound @page-state) "" " game-over"))
          :on-click (fn [e] (toggle-sound!) (.preventDefault e))
          :title (if (:mute-sound @page-state) "Enable sound" "Disable sound")}
      [:i {:class "fa fa-volume-up"}] " "]
     [:a {:class (str "button"
                      (if (= (:state @app-state) :playing)
                        ""
                        " game-over"))
          :on-click (fn [e] (new-game!) (.preventDefault e))
          :href "#"}
      (if (not= (:state @app-state) :playing) "New Game" "Restart")]
     [:a {:href "#"
          :class (str "button" (if (:mute-music @page-state) "" " game-over"))
          :on-click (fn [e] (toggle-music!) (.preventDefault e))
          :title (if (:mute-music @page-state) "Play music" "Mute music")}
      [:i {:class "fa fa-music"}] " "]
     ]
    
    [:div {:style {:font-size 12}}
     (bottom-link "#" :game "game" (:bottom-section @page-state))
     (bottom-link "#" :about "about" (:bottom-section @page-state))
     (bottom-link "#" :rules "rules" (:bottom-section @page-state))
     (bottom-link "#" :instructions "playing" (:bottom-section @page-state))
     (bottom-link "#" :credits "credits" (:bottom-section @page-state))
     (bottom-link "https://github.com/Zenith-One/thrones-and-bones-cljs" :none "source" nil false)]
    ]
   [:div {:class "bottom-content-container"}
    (bottom-content
     :game (:bottom-section @page-state)
     (let [board (:board @app-state)
           pieces (:pieces @app-state)
           state (:state @app-state)
           valid-moves (:valid-moves @app-state)
           squares (render-squares board valid-moves state)
           rendered-pieces (render-pieces pieces (:selected @app-state))
           turn-indicator (render-turn-indicator (:turn @app-state) (:state @app-state))
           base [:svg {:view-box "0 0 11 12"
                       :width "100%"
                       :style {:margin-top 20
                               :max-width 500}}]]
       (into base
             (apply conj
                    (apply conj
                           (apply conj rendered-pieces (render-labels))
                           turn-indicator)
                    squares))))

    (bottom-content :about (:bottom-section @page-state)
                    [:div
                     [:h2 "A new take on an ancient game"]
                     [:p "Thrones and Bones is an asymmetric board game created by "
                      [:a {:href "http://www.louanders.com/"
                           :target "_blank"} "Lou Anders"]
                      " for his Thrones and Bones series. By asymmetric, I mean the play rather than the board. The white pieces are the Jarl (the white square, pronounced \"yarl\") and his guards. They are trying to get the Jarl safely to the edge of the board. Meanwhile the Black Draug (black square) and his draug minions are trying to capture the Jarl."]
                     [:p "It is based on a medieval game called Tablut, which is in turn based on a still older game called Hnefatafl (game of the fist, or king's table). Some historians believe that, before chess took the world by storm, Hnefatafl and its many variants were the most popular board games in the western world."]
                     [:p "If you are interested in learning more about Hnefatafl, I suggest checking out "
                      [:a {:href "http://tafl.cyningstan.com/"
                           :target "_blank"}
                       "Hnefatafl - The Game of the Vikings"]
                      ". There is a lot of really interesting information there."]])

    (bottom-content :rules (:bottom-section @page-state)
                    [:div
                     [:h2 "Rules"]
                     [:p "The majority of the rules for this game are standard for hnefatafl games in general, which you can check out "
                      [:a {:href "http://tafl.cyningstan.com/page/20/a-rule-book-for-hnefatafl"} "here"] ". Here are the basics:"
                      [:ol
                       [:li "The Jarl player (white) goes first. The Draug player (black) goes second."]
                       [:li "Pieces move up, down, left, or right as many squares as they wish."]
                       [:li "Pieces may not move through other pieces."]
                       [:li "You may capture one or more enemy pieces by moving in such a way as to have an enemy piece sandwiched between two of yours."
                        [:ol {:type "a"}
                         [:li "Captures are only actively made, so if you move one of your pieces between two enemy pieces, you are not immediately captured."]
                         [:li "A single move may capture more than one enemy piece if that move causes more than one piece to be individually sandwiched between your pieces"]]]
                       [:li "The Jarl escapes when he makes it to one of the normal squares at the edge of the board."]
                       [:li "The draug win when the Jarl is captured (see below)."]
                       [:li "If at any time one side has no valid moves, or if the only valid move would cause the same series of moves to be made by both players, it is a draw."]]]
                     [:p "Thrones and Bones adds several new elements to the game:"
                      [:ol
                       [:li "The Black Draug may move to any unoccupied square on the board. This is an exception to every move restriction imposed on other pieces."]
                       [:li "However, if The Black Draug is captured, the Jarl wins."]
                       [:li "The Jarl's starting square is the castle. Any white piece may move through the castle, but only the Jarl may stop there."]
                       [:li "The draug starting squares are burial mounds. No piece may move onto or over a burial mound, unless that piece is a draug who has not yet moved onto a normal square."]
                       [:li "The Jarl can be captured if he is surrounded by any combination of draug pieces and non-normal terrain (e.g. castle or burial mounds)."]]]])
    (bottom-content :instructions (:bottom-section @page-state)
                    [:div
                     [:h2 "How to play"]
                     [:ul
                      [:li "When it is your turn, click on one of your pieces."]
                      [:li "All of the valid moves for that piece will be highlighted."]
                      [:li "To complete your move, click on one of the highlighted squares. This will complete the move, perform any captures, and end the turn."]
                      [:li "If at any time you wish to start a new game, click the \"Restart\" button at the top."]]])
    (bottom-content :credits (:bottom-section @page-state)
                    [:div
                     [:h2 "Credits"]
                     [:p "Of course, the most credit for this game goes to "
                      [:a {:href "http://www.louanders.com/" :target "_blank"}
                       "Lou Anders "]
                      "for making this variant of Hnefatafl, and in turn the various "
                      "people and peoples involved with "
                      [:a {:href "http://tafl.cyningstan.com/page/3/the-history-of-hnefatafl" :target "_blank"} "the history of Hnefatafl"] "."]
                     [:p "Additionally, some of the original inspiration for making "
                      "this style of game the way I have made it came from a "
                      [:a {:href "https://www.youtube.com/watch?v=pIiOgTwjbes"
                           :target "_blank"} "video "]
                      "on how to implement Tic-Tac-Toe in ClojureScript."]
                     [:p "Finally, I used several resources which I did not "
                      "create to get the job done:"]
                     [:h3 "Code-Related"]
                     [:ul
                      [:li
                       [:a {:href "https://www.gnu.org/software/emacs/"
                            :target "_blank"}
                        "GNU Emacs"]
                       " with "
                       [:a {:href "https://github.com/bbatsov/prelude"
                            :target "_blank"}
                        "Prelude"]]
                      [:li
                       [:a {:href "https://leiningen.org/"
                            :target "_blank"} "Leiningen"]]
                      [:li [:a {:href "https://github.com/bhauman/lein-figwheel"
                                :target "_blank"}
                            "Figwheel"]]]
                     [:h3 "Media Assets"]
                     [:ul
                      [:li [:a {:href "http://fontawesome.io"
                                :target "_blank"} "Font Awesome by Dave Gandy"]]
                      [:li
                       [:a {:href "https://www.freesound.org/people/ninebilly/sounds/173018/"
                            :target "_blank"} "Wood Door by ninebilly"]
                       " - Edited and used for piece clicks"]
                      [:li
                       [:a {:href "https://www.freesound.org/people/Matt%20Namer/sounds/93750/"
                            :target "_blank"} "War Drum Single Hits by Matt Namer"]
                       " - Edited and used for the victory sounds "
                       [:a {:href "https://creativecommons.org/licenses/sampling+/1.0/"
                            :target "_blank"} "(License)"]]
                      [:li
                       [:a {:href "https://www.freesound.org/people/Kyster/sounds/197113/"
                            :target "_blank"} "An Dro by Kyster"]
                       " - Edited and mixed into the background "
                       [:a {:href "https://creativecommons.org/licenses/by/3.0/"
                            :target "_blank"} "(License)"]]
                      [:li
                       [:a {:href "https://www.freesound.org/people/Robinhood76/sounds/209354/"
                            :target "_blank"} "Tavern Ambience - Looping by Robinhood76"]
                       " - Edited and mixed into the music "
                       [:a {:href "https://creativecommons.org/licenses/by-nc/3.0/"
                            :target "_blank"} "(License)"]]]])]
   ;; ======== sound/music ========
   ;; wood door - https://www.freesound.org/people/ninebilly/sounds/173018/
   ;; war drums - https://www.freesound.org/people/Matt%20Namer/sounds/93750/
   ;; an dro - https://www.freesound.org/people/Kyster/sounds/197113/
   ;; tavern ambience - https://www.freesound.org/people/Robinhood76/sounds/209354/

   ])

(reagent/render-component [thrones-and-bones]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
