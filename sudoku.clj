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

(let [boxes (for [_ coords] 0x3fe)]
  (def empty-board (vec (concat boxes [0]))))

(defmacro boxes [board]
  `(subvec ~board 0 81))
(defmacro inc-solved-no [board]
  `(let [board# ~board]
     (assoc board# 81 (inc (board# 81)))))
(defmacro solvedno [board]
  `(~board 81))

(defmacro int-to-bit [number]
  `(bit-shift-left 1 ~number))
(defmacro bit-to-int [number]
  `(Integer/numberOfTrailingZeros ~number))

(defmacro without [board coord possibility]
  `(bit-and (~board ~coord) (bit-not ~possibility)))
(defmacro contains-possibility? [board coord possibility]
  `(not (zero? (bit-and (~board ~coord) ~possibility))))
(defmacro number-of-possibilities [possibility]
  `(Integer/bitCount ~possibility))
(letfn
    [(e [possibilities]
        (if (zero? possibilities)
          '()
          (let [number (int-to-bit (Integer/numberOfTrailingZeros possibilities))]
            (cons number (e (bit-and possibilities (bit-not number)))))))]
  (def each (vec (for [n (range 0x3fe)] (e n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mark a number on board. Eliminate a number from the set of possibilities.

(declare eliminate)

(defn mark
  ([board element]
     (mark board (element 0) (element 1)))
  ([board coord value]
     (reduce #(eliminate %1 coord %2) board (each (without board coord value)))))

(defn eliminate-from-neighbours [board coord value]
  (reduce #(eliminate %1 %2 value) board (neighbours-of coord)))

(defn eliminate [board coord possibility]
  (if (contains-possibility? board coord possibility)
    (let [possibilities (without board coord possibility)
          board (assoc board coord possibilities)
          size (number-of-possibilities possibilities)]
      (cond
        ;; Contradiction.
        (== size 0) (throw (Error.))
        ;; Naked single method
        ;; http://www.sadmansoftware.com/sudoku/nakedsingle.htm
        (== size 1) (inc-solved-no (eliminate-from-neighbours board coord possibilities))
        ;; Hidden single method
        ;; http://www.sadmansoftware.com/sudoku/hiddensingle.htm
        true        (loop [groups (groups-of coord)]
                      (if-let [group (first groups)]
                        (let [coords-with-value (filter #(contains-possibility? board % possibility) group)]
                          (cond
                            (empty? coords-with-value)        (throw (Error.))
                            (empty? (rest coords-with-value)) (mark board (first coords-with-value) possibility)
                            true                              (recur (rest groups))))
                        board))))
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
      (let [cnt (number-of-possibilities value)]
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
              (each values))))
       board))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading the board from file.

(defn- read-marks [filename]
  (for [[c v] (zipmap
               coords
               (seq (.split #"\s+" (slurp filename))))
        :when (re-matches #"[0-9]+" v)]
    [c (int-to-bit (Integer/parseInt v))]))

(defn read-board [filename]
  (reduce mark empty-board (read-marks filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing the board.

(defn- board-as-string [board]
  (let [elements (for [v (boxes board)] (if (== (number-of-possibilities v) 1) (str (bit-to-int v)) "."))]
    (join "\n" (for [row (partition 9 elements)] (join " " row)))))

(defn print-board [board]
  (println (board-as-string board)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main.

(defn main
  ([progname]
     (printf "Usage: %s puzzle-file...%n" progname))
  ([_ & filenames]
     (let [start (.. System nanoTime)]
       (doseq [filename filenames]
         (println filename)
         (print-board (solve (read-board filename))))
       (println
        (format "Solved %d puzzles in %.3f seconds."
                (count filenames)
                (/ (- (.. System nanoTime) start) 1000000000.0))))))

;; Hack until clojure allows to differentiate between running a file as
;; a script and loading it from another module.
(when (= "sudoku.clj" (first *command-line-args*))
  (apply main *command-line-args*)
  (System/exit 0))
