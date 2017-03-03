(ns thrones-bones.core
  (:require [reagent.core :as reagent :refer [atom]]
            [thrones-bones.rendering :refer [app-component]]
            [thrones-bones.game-logic :refer [app-state]]))

(enable-console-print!)

(reagent/render-component [app-component]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  #_(println "selected" (:selected @app-state))
  #_(println app-state)
  #_(println (swap! app-state assoc :blah 1234))
  #_(println (empty? (rest '(1))))
  (println )
)
