(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board [y x]]
  (get-in board [y x]))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board [row]]
  (reduce #(conj %1 %2) #{} (get board row)))

(defn col-values [board [_ col]] (reduce #(conj %1 (get %2 col)) #{} board))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn proper-coord-pairs [rows columns]
  (for [row rows
        col columns]
    [row col]))

(defn top-left [[row col]]
  [(* (int (/ row 3)) 3) (* (int (/ col 3)) 3)])

(defn block-values [board coord]
  (let [[top-row left-col] (top-left coord)
        rows (range top-row (+ top-row 3))
        cols (range left-col (+ left-col 3))
        coords (proper-coord-pairs rows cols)]
    (reduce
      (fn [acc [row col]]
        (conj acc (get-in board [row col])))
      #{}
      coords)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
      all-values
      (set/union (block-values board coord)
                 (row-values board coord)
                 (col-values board coord)))))

(defn all-numbers [board]
  (reduce concat board))

(defn filled? [board]
  (every? #(< 0 %) (all-numbers board)))

(defn rows [board]
  (loop [acc []
         n 0]
    (if (>= n (count board))
      acc
      (recur
        (conj acc (row-values board [n 0]))
        (inc n)))))

(defn cols [board]
  (loop [acc []
         n 0]
    (if (>= n (count board))
      acc
      (recur
        (conj acc (col-values board [0 n]))
        (inc n)))))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
    (block-values board [x y])))

(defn valid-rows? [board]
  (every?
    (fn [row-values]
      (= (set/difference all-values row-values) #{}))
    (rows board)))

(defn valid-cols? [board]
  (every?
    (fn [values]
      (= (set/difference all-values values) #{}))
    (cols board)))

(defn valid-blocks? [board]
  (every?
    (fn [values]
      (= (set/difference all-values values) #{}))
    (blocks board)))

(defn valid-solution? [board]
  (and
    (valid-blocks? board)
    (valid-rows? board)
    (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (proper-coord-pairs (range 0 (count board)) (range 0 (count (first board))))]
    (some
      #(if ((complement has-value?) board %)
        %
        false)
      coords)))

(defn sum [a-seq]
  (reduce + a-seq))

(defn subset-sum-helper [a-set current-set target]
  (if (= (sum current-set) target)
    [current-set]
    (let [remaining (clojure.set/difference a-set current-set)]
      (for [elem remaining
            solution (subset-sum-helper a-set
                                        (conj current-set elem)
                                        target)]
        solution))))

(defn subset-sum [a-set target]
  (first (subset-sum-helper a-set #{} target)))

(defn solve-helper [current-board]
  (if (filled? current-board)
    (if  (valid-solution? current-board)
      [current-board]
      '())
    (let [empty-location (find-empty-point current-board)]
      (for [option (valid-values-for current-board empty-location)
            solution (solve-helper (set-value-at current-board empty-location option))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
