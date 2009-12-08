(ns sudoku
  (:use [clojure.set :only (union)]
        [clojure.contrib.str-utils2 :only (join)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Board representation.

(def coords (range 0 81))
(let [blocks (apply concat
                    (map
                     (fn [x]
                       (map #(apply concat %)
                            (partition 3 (apply interleave (partition 3 x)))))
                     (partition 9 (partition 3 coords))))
      rows (partition 9 coords)
      columns (partition 9 (apply interleave rows))]
  (def groups (concat blocks rows columns)))

(def groups-of
     (into {}
           (for [c coords]
             [c (for [coords groups :when (contains? (set coords) c)] coords)])))
(def neighbours-of
     (into {}
           (for [c coords]
             [c (vec (disj (apply union (map set (groups-of c))) c))])))

(let [boxes (for [_ coords] (set (range 1 10)))]
  (def empty-board (vec (concat boxes [0]))))

(defmacro update [map key f]
  `(let [map# ~map key# ~key]
     (assoc map# key# (~f (map# key#)))))
(defmacro boxes [board]
  `(subvec ~board 0 81))
(defmacro update-coord [board coord f]
  `(update ~board ~coord ~f))
(defmacro inc-solved-no [board]
  `(update ~board 81 inc))
(defmacro solvedno [board]
  `(~board 81))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mark a number on board. Eliminate a number from the set of possibilities.

(declare eliminate)

(defn mark
  ([board element]
     (mark board (element 0) (element 1)))
  ([board coord value]
     (reduce #(eliminate %1 coord %2) board (disj (board coord) value))))

(defn eliminate-from-neighbours [board coord value]
  (reduce #(eliminate %1 %2 value) board (neighbours-of coord)))

(defn eliminate [board coord possibility]
  (if (contains? (board coord) possibility)
    (let [board (update-coord board coord #(disj % possibility))
          possibilities (board coord)
          size (count possibilities)]
      (cond
        ;; Contradiction.
        (== size 0) (throw (Error.))
        ;; Naked single method
        ;; http://www.sadmansoftware.com/sudoku/nakedsingle.htm
        (== size 1) (inc-solved-no (eliminate-from-neighbours board coord (first possibilities)))
        ;; Hidden single method
        ;; http://www.sadmansoftware.com/sudoku/hiddensingle.htm
        true        (or
                     (reduce
                      (fn [result group]
                        (or
                         result
                         (let [coords-with-value (filter #(contains? (board %) possibility) group)]
                           (cond
                             (empty? coords-with-value)        (throw (Error.))
                             (empty? (rest coords-with-value)) (mark board (first coords-with-value) possibility)
                             true                              nil))))
                      nil
                      (groups-of coord))
                     board)))
    board))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solve.

(defn element-with-least-possibilities [board]
  (loop [min-coord nil
         min-val nil
         min-cnt 10
         i 0
         values (boxes board)]
    (if-let [value (first values)]
      (let [cnt (count value)]
        (cond
          (== cnt 2)        [i value]
          (< 1 cnt min-cnt) (recur i value cnt (inc i) (rest values))
          true              (recur min-coord min-val min-cnt (inc i) (rest values))))
      [min-coord min-val])))

(defn- solved? [board]
  (== (solvedno board) 81))

(defn solve [board]
  (if (solved? board)
    board
    (let [[coord values] (element-with-least-possibilities board)]
      (or
       (first (filter solved?
             (map
              #(try
                (solve (mark board coord %))
                (catch Error e board))
              values)))
       board))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading the board from file.

(defn- read-marks [filename]
  (for [[c v] (zipmap
               coords
               (seq (.split #"\s+" (slurp filename))))
        :when (re-matches #"[0-9]+" v)]
    [c (new Integer v)]))

(defn read-board [filename]
  (reduce mark empty-board (read-marks filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing the board.

(defn- board-as-string [board]
  (let [elements (for [v (boxes board)] (if (== (count v) 1) (str (first v)) "."))]
    (join "\n" (for [row (partition 9 elements)] (join " " row)))))

(defn print-board [board]
  (println (board-as-string board)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main.

(defn main
  ([progname]
     (printf "Usage: %s puzzle-file...%n" progname)
     (System/exit 0))
  ([_ & filenames]
     (let [start (.. System nanoTime)]
       (doseq [filename filenames]
         (println filename)
         (print-board (solve (read-board filename))))
       (println
        (format "Solved %d puzzles in %.3f seconds."
                (count filenames)
                (/ (- (.. System nanoTime) start) 1000000000.0)))
       (System/exit 0))))

;; Hack until clojure allows to differentiate between running a file as
;; a script and loading it from another module.
(if (= "sudoku.clj" (first *command-line-args*))
  (apply main *command-line-args*))
