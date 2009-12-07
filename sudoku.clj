(ns sudoku
  (:use [clojure.set :only (union)]
        [clojure.contrib.str-utils2 :only (join)]
        [clojure.test :only (is run-tests with-test)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Board representation.

(def coords (for [x (range 1 10) y (range 1 10)] [x y]))
(let [blocks (for [i [1 4 7] j [1 4 7]]
               (for [x (range i (+ i 3)) y (range j (+ j 3))] (keyword (str [x y]))))
      rows (partition 9 (map #(keyword (str %)) coords))
      columns (partition 9 (for [[x y] coords] (keyword (str [y x]))))]
  (def groups (concat blocks rows columns)))
(def coords (map #(keyword (str %)) coords))

(def groups-of
     (into {}
           (for [c coords]
             [c (for [coords groups :when (contains? (set coords) c)] coords)])))
(def neighbours-of
     (into {}
           (for [c coords]
             [c (vec (disj (apply union (map set (groups-of c))) c))])))

(defstruct board :boxes :solvedno)
(let [boxes (into {} (for [coord coords] [coord (set (range 1 10))]))]
  (def empty-board
       (struct-map board :boxes boxes :solvedno 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers

(defmacro update [map key f]
  `(let [map# ~map key# ~key]
     (assoc map# key# (~f (map# key#)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mark a number on board. Eliminate a number from the set of possibilities.

(declare eliminate)

(defn mark
  ([board element]
     (mark board (element 0) (element 1)))
  ([board coord value]
     (reduce #(eliminate %1 coord %2) board (disj ((board :boxes) coord) value))))

(defn eliminate-from-neighbours [board coord value]
  (reduce #(eliminate %1 %2 value) board (neighbours-of coord)))

(defn eliminate [board coord possibility]
  (if (contains? ((board :boxes) coord) possibility)
    (let [board (update-in board [:boxes coord] disj possibility)
          possibilities ((board :boxes) coord)
          size (count possibilities)]
      (cond
        ;; Contradiction.
        (== size 0) (throw (Error.))
        ;; Naked single method
        ;; http://www.sadmansoftware.com/sudoku/nakedsingle.htm
        (== size 1) (update
                     (eliminate-from-neighbours board coord (first possibilities))
                     :solvedno inc)
        ;; Hidden single method
        ;; http://www.sadmansoftware.com/sudoku/hiddensingle.htm
        true        (or
                     (reduce
                      (fn [result group]
                        (or
                         result
                         (let [coords-with-value (filter #(contains? ((board :boxes) %) possibility) group)]
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
  (apply min-key #(count (val %))
         (filter #(> (count (val %)) 1) (board :boxes))))

(defn- solved? [board]
  (== (board :solvedno) 81))

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
  (let [elements (for [[c v] (sort (board :boxes))] (if (== (count v) 1) (str (first v)) "."))]
    (join "\n" (for [row (partition 9 elements)] (join " " row)))))

(defn print-board [board]
  (println (board-as-string board)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main.

(defn main
  ([progname]
     (printf "Usage: %s puzzle-file...%n" progname)
     (run-tests)
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
