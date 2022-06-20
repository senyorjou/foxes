(ns foxes.world
  (:require [clojure.set :as set]))

(def world (atom {}))

(def world-sample {[0 0] {:terrain :water}
                   [0 1] {:terrain :rock}})


(def terrain {:water   {:color "#bcf4f5"}
              :grass-1 {:color "#ccfccb"}
              :grass-2 {:color "#96e6b3"}
              :grass-3 {:color "#568259"}
              :rock    {:color "#8f8073"}})

(defn seq->world [points terrain]
  "Applies any point sequence to world and assigns a terrain"
  (apply array-map (mapcat (fn [p] [p terrain]) points)))


(defn get-terrain [terrain-type]
  "Creates a terrain record for a given terrain-type"
  (-> terrain
      (get terrain-type)
      (assoc :terrain terrain-type)))

(defn within-bounds
  "ensures a new point is within bounds"
  [x y w h]
  (and (<= 0 x (dec w))
       (<= 0 y (dec h))))


(defn rocks
  "Create rocks"
  [w h world num-rocks]
  (take num-rocks (repeatedly (fn [] [(rand-int w) (rand-int h)]))))


(defn neighbours
  "Find adjacent squares of x y within world"
  [x y w h]
  (for [dx (range -1 2)
        dy (range -1 2)
        :let [new-x (+ x dx)
              new-y (+ y dy)]
        :when (and (not (and (= dx 0) (= dy 0)))
                   (within-bounds new-x new-y w h))]
    [new-x new-y]))


(defn orbit
  "Reveal sorrounding points of a x y location"
  [x y world]
  (let [points (for [dx (range -1 2)
                     dy (range -1 2)
                     :when (not (and (= dx 0) (= dy 0)))]
                 (get world [(- x dx) (- y dy)]))]
    (filter some? points)))

(defn squares-of
  "Returns all squares of a certain terrain"
  [terrain-type world]
  (map first (filter (fn [[_ v]] (= (:terrain v) terrain-type)) world)))

(defn habitable
  "Finds all squares that are habitable (not water...)"
  [world]
  (map first (filter
              (fn [[_ v]]
                (some? (#{:grass-1 :grass-2 :grass-3 :rock} (:terrain v))))
              world)))


(defn empty-neighbours
  "Reveals empty neighbours of a source terrain"
  [source world w h]
  (let [source-points (squares-of source world)
        source-neighbours (distinct (apply concat (map (fn [[x y]] (neighbours x y w h)) source-points)))
        void-sq (remove #(:terrain (get world %)) source-neighbours)]
    void-sq))

(defn find-empty
  "Gets all empty squares"
  [world w h]
  (for [x (range w)
        y (range h)
        :when (nil? (get world [x y]))]
    [x y]))


(defn fill-natural
  "Randomly fills squares"
  [squares]
  (let [grass-terrain (vec (apply dissoc terrain [:grass-3 :water :rock]))]
    (reduce (fn [acc sq] (assoc acc sq (get-terrain (first (rand-nth grass-terrain))))) {} squares)))


(defn lake
  "creates a lake at random pos within 0, w, h"
  [w h lake-size]
  (let [initial-x (rand-int w)
        initial-y (rand-int h)]
    (loop [lake-set #{[initial-x initial-y]}
           max-iter 0]
      (if (or (>= (count lake-set) lake-size)
              (> max-iter 10))
        lake-set
        ;; get any point of existing lake
        (let [[x y] (rand-nth (vec lake-set))
              new-x (+ x (dec (rand-int 3)))
              new-y (+ y (dec (rand-int 3)))]
          (if (within-bounds new-x new-y w h)
            (recur (conj lake-set [new-x new-y]) max-iter)
            (recur lake-set (inc max-iter))))))))

(defn- aprox
  "Returns a random size of aprox min-size and max-size"
  ([] (aprox 15))
  ([max-size]
  (let [min-size 3]
    (+ min-size (rand-int (- max-size min-size))))))


(defn generate-world
  "Creates a world for nice creatures"
  [w h]
  (let [num-lakes (+ 2 (rand-int 5))
        water (apply set/union (take num-lakes (repeatedly #(lake w h (aprox 30)))))]
    (-> (seq->world water (get-terrain :water))
        (merge (seq->world (rocks w h {} 30) (get-terrain :rock)))
        (#(merge % (seq->world (empty-neighbours :water % w h) (get-terrain :grass-3))))
        (#(merge % (seq->world (empty-neighbours :grass-3 % w h) (get-terrain :grass-2))))
        (#(merge % (seq->world (empty-neighbours :grass-2 % w h) (get-terrain :grass-1))))
        (#(merge % (fill-natural (find-empty % w h)))))))
