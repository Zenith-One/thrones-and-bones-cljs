(ns thrones-bones.util)

;; copied from:
;; https://clojuredocs.org/clojure.core/conj#example-56a6799ee4b060004fc217b0
(def vec-join (comp vec flatten conj))

(defn exists-in? [seq item]
  (if (or (nil? seq) (nil? item))
    false
    (contains? (zipmap seq (repeat true)) item)))

