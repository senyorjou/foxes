(ns foxes.main
  (:require [goog.dom :as dom]
            [foxes.world :as w]))

(js/console.log "Hello foxie world!")
(def ctx (.getContext (dom/getElement "canvas") "2d"))
(def sq-size 30)
(def world-x 20)  ;; squares per x
(def world-y 20)  ;; squares per y
(def offset (/ sq-size 2))


(def game-state (atom {:sqs []}))

(defn world-position [x y]
   (map #(+ offset (* sq-size %)) [x y]))


(defn stats [world]
  (count world))


(defn sq-bounds [x y]
  "Bounds to draw a svg rect"
  (let [px (* sq-size x)
        py (* sq-size y)]
    [px py sq-size sq-size]))


(defn draw-sq! [x y color]
  "draws a square  of a given color at x y"
  (let [[x y dx dy] (sq-bounds x y)]
    (set! (.-fillStyle ctx) color)
    (.beginPath ctx)
    (.rect ctx x y dx dy)
    (.fill ctx)))

(defn draw-rabbit! [x y]
  (let [center (/ sq-size 2)
        [x y dx dy] (sq-bounds x y)
        center-x (+ x center)
        center-y (+ y center)]
    (set! (.-fillStyle ctx) "#eee")
    (.beginPath ctx)
    (.arc ctx center-x center-y 10 0 (* 2 Math.PI))
    (.stroke ctx)
    (.fill ctx)))


;; arc(x, y, radius, startAngle, endAngle)

(defn draw-world! [w h world]
  "Draws the world according to current map"
  (set! (.-height (dom/getElement "canvas")) (+ 1 (* h sq-size)))
  (set! (.-width (dom/getElement "canvas")) (+ 1 (* w sq-size)))
  (doall
   (map (fn [[[x y] v]] (draw-sq! x y (:color v))) world)))

(defn draw-rabbits [rabbits]
  (doall
   (map (fn [[x y]] (draw-rabbit! x y)) rabbits)))


(def world (w/generate-world 20 20))
(def rabbits [[10 10]])

(defn move-rabbit [rabbit world]
  ;; find a spot in the world
  (let [[x y] rabbit
        places (w/habitable (select-keys world (w/neighbours x y 20 20)))]
    (rand-nth places)))

(defn place-rabbits
  "Places rabbits randomly in map"
  [world]
  (let [habitats (w/habitable world)]
    (take 10 (shuffle habitats))))


(defn move-rabbits [rabbits world]
   (map #(move-rabbit % world) rabbits))


(defn sleep [msec]
  (js/setTimeout (fn []) msec))

;; (draw-world! world-x world-y world)

(loop [rabbits (place-rabbits world)
       iter 0]
  (if (< 30 iter)
    iter
    (do
      (draw-world! world-x world-y world)
      (draw-rabbits rabbits)
      (println (first rabbits))
      (sleep 1000)
      (recur (move-rabbits rabbits world)
             (inc iter)))))

(comment
  (draw-world! world-x world-y (w/generate-world-2 20 20))
  (w/lake 20 20 20)

  (def world (w/seq->world (w/lake 20 20 6) (w/get-terrain :water)))
  (def rabbits (place-rabbits world))
  (draw-world! 20 20 world)

  (first world)
  (count rabbits)

  (reduce (fn [acc [k v]] (assoc acc k (count v))) {} (group-by (fn [[_ cell]] (:terrain cell)) world))

  (draw-rabbit! 10 10)

  (move-rabbits rabbits world))
