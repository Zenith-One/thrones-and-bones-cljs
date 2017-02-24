(ns thrones-bones.util)

;; copied from:
;; https://clojuredocs.org/clojure.core/conj#example-56a6799ee4b060004fc217b0
(def vec-join (comp vec flatten conj))

(defn exists-in? [seq item]
  (if (or (nil? seq) (nil? item))
    false
    (contains? (zipmap seq (repeat true)) item)))

(defn toggle! [a k]
  "Given atom a and key k whose value is a boolean, 
   swaps k in a with the opposite value, and returns the new value of (k a)"
  (k (swap! a assoc k (not (k @a)))))
