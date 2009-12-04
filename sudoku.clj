(ns sudoku
  (:use [clojure.contrib.str-utils2 :only (join)]
        [clojure.contrib.seq-utils :only (frequencies)]
        [clojure.test :only (is run-tests with-test)]))

(with-test
 (defn find-uniq
   "From a collection of values return one that appears only once."
   [values]
   (first
    (for [[value counter] (frequencies values)
          :when (= counter 1)]
      value)))
 (is (=         2      (find-uniq [1 2 1])))
 (is (=         nil    (find-uniq [1 2 1 2])))
 (is (contains? #{1 2} (find-uniq [1 2])))
 (is (=         nil    (find-uniq []))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Board representation.
;;
;; Board is a sorted map of coordinates (pairs of numbers, e.g. [3 5]) to
;;   values (either an integer or set of possible numbers).

(def coords (for [x (range 1 10) y (range 1 10)] [x y]))
(def blocks (for [i [1 4 7] j [1 4 7]] (into #{} (for [x (range i (+ i 3)) y (range j (+ j 3))] [x y]))))
(def rows (partition 9 coords))
(def columns (partition 9 (for [[x y] coords] [y x])))
(def groups (concat blocks rows columns))

(defn same-row? [c1 c2]
  (= (first c1) (first c2)))
(defn same-col? [c1 c2]
  (= (second c1) (second c2)))
(defn same-block? [c1 c2]
  (and (= (quot (- (first c1) 1) 3) (quot (- (first c2) 1) 3))
       (= (quot (- (second c1) 1) 3) (quot (- (second c2) 1) 3))))
(defn neighbours? [c1 c2]
  (some #(% c1 c2) [same-row? same-col? same-block?]))

(def empty-board
     (into (sorted-map)
           (for [coord coords]
             [coord (set (range 1 10))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mark a number on board.

(defn mark
  ([board element]
     (mark board (first element) (second element)))
  ([board coord value]
     (into (sorted-map)
           (for [[c v] board]
             (cond
               (= c coord)                          [c value]
               (and (set? v) (neighbours? c coord)) [c (disj v value)]
               true                                 [c v])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Naked single method
;; http://www.sadmansoftware.com/sudoku/nakedsingle.htm

(defn- naked-single? [element]
  (let [value (val element)]
    (and (set? value) (= (count value) 1))))

(defn solve-with-naked-single [board]
  (if-let [element (first (filter naked-single? board))]
    (mark board (key element) (first (val element)))
    board))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hidden single method
;; http://www.sadmansoftware.com/sudoku/hiddensingle.htm

(defn group-values [board group]
  (apply concat (remove number? (map board group))))

(defn contains-possibility? [cell possibility]
  (let [value (second cell)]
    (and (not (number? value))
         (contains? value possibility))))

(defn cell-with-possibility [cells value]
  (first (first (filter #(contains-possibility? % value) cells))))

(defn hidden-singles [board]
  (for [group groups
        :let [val (find-uniq (group-values board group))]
        :when (not (nil? val))]
    [(cell-with-possibility (for [coord group] [coord (board coord)]) val) val]))

(defn solve-with-hidden-single [board]
  (if-let [element (first (hidden-singles board))]
    (mark board element)
    board))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Guessing method.

(declare solve solved?)

(defn element-with-least-possibilities [board]
  (first (sort #(compare (count (val %1)) (count (val %2)))
               (filter #(set? (val %)) board))))

(defn solve-by-guessing [board]
  (let [element (element-with-least-possibilities board)]
    (or (first (filter solved? (map solve (map #(mark board (key element) %) (val element)))))
        board)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solve function that combines all methods.

(defn- solved? [board]
  (every? number? (vals board)))

(defn- solve-step [board]
  (let [new-board (solve-with-hidden-single (solve-with-naked-single board))]
    (if (= new-board board)
      (solve-by-guessing board)
      new-board)))

(defn solve [board]
  (if (solved? board)
    board
    (let [new-board (solve-step board)]
      (if (= board new-board)
        board
        (recur new-board)))))

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
  (let [elements (for [[c v] board] (if (number? v) (str v) "."))]
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
