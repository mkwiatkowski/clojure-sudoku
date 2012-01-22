(use 'sudoku)

(defn perf [_ & filenames]
  (let [boards (vec (map read-board filenames))]
    (let [start (.. System nanoTime)]
      (doseq [board boards]
        (solve board))
      (/ (- (.. System nanoTime) start) 1000000000.0))))

(defn m [times]
  (let [results (vec (map (fn [_] (apply perf *command-line-args*)) (range times)))]
    (println results)
    (println "min:" (first (sort results)))
    (println "max:" (last (sort results)))
    (println "average:" (/ (reduce + 0 results) times))
    (println "median:" (nth (sort results) (/ times 2)))))

(m 3)
(m 20)
