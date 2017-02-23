(ns thrones-bones.rendering
  (:require [reagent.core :as reagent :refer [atom]]
            [thrones-bones.game-logic :as logic :refer [page-state app-state]]
            [thrones-bones.util :as util :refer [vec-join exists-in?]]))


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

;; ========== RENDERING ==========
;; Piece Rendering
(defn circle-conf [x y fill selected click-handler]
  {:r 0.3 :cx (+ x 0.45) :cy (+ y 0.45)
   :fill fill :stroke (if selected "#f00"  "#000")
   :stroke-width 0.015
   :on-click #(click-handler (- x 1) (- y 2))})

(defn rect-conf [x y fill selected click-handler]
  {:height 0.7 :width 0.7 :x (+ x 0.1) :y (+ y 0.1)
   :fill fill :stroke (if selected "#f00" "#000")
   :stroke-width 0.015
   :on-click #(click-handler (- x 1) (- y 2))})

(defn render-piece [selected click-handler piece]
  (let [[ox oy] (:coords piece)
        [t c] (if (:leader piece)
                [:rect rect-conf]
                [:circle circle-conf])
        [sx sy] selected
        y (+ oy 2)
        x (+ ox 1)
        selected (logic/is-the-piece-here? piece sx sy)]
    (case (:team piece)
      :black [t (c x y black-color selected click-handler)]
      :white [t (c x y white-color selected click-handler)]
      nil)))

(defn render-pieces [pieces selected click-handler]
  (map (partial render-piece selected click-handler) pieces))

;; Board Rendering
(defn render-square [x y color stroke click-handler]
  [:rect {:width 0.9
          :height 0.9
          :fill color
          :x x
          :y y
          :stroke stroke
          :stroke-width 0.015
          :on-click #(click-handler (- x 1) (- y 2))
          }])

(defn render-squares [board valid-moves state click-handler]
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
         "#666")
       click-handler))))

(defn render-turn-indicator-circle [color state]
  (render-piece [nil nil] {:coords [(if (= color :white) 2 6) -2]
                    :team color
                           :leader false}
                #()))

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
    (logic/stop-sound :music)
    (logic/play-sound :music)))

(defn toggle-sound! []
  (swap! page-state assoc :mute-sound (not (:mute-sound @page-state))))

;; App Component
(defn app-component []
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
          :on-click (fn [e] (logic/new-game!) (.preventDefault e))
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
           squares (render-squares board valid-moves state logic/handle-click-square!)
           rendered-pieces (render-pieces pieces (:selected @app-state)
                                          logic/handle-click-piece!)
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




