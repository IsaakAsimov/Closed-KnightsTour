(ns ckt.core
   (:gen-class))

(defn isin?
  "Returns true if x is an element of list,
   and nil if is not."
  [list x]
  (some #(= x %) list))

(defn board-f
  "board-f(3) = [[0 0 0] [0 0 0] [0 0 0]].
  Returns a vector of length size with vectors
  of length size inside."
  [size]
  (vec (repeat size (vec (repeat size 0)))))

(defn options
  "Returns every possible options that a knight
  have in certain position of certain board."
  [board [y x]]
  (for [[v h] [[ 1  2] [ 1 -2] [-1  2] [-1 -2]
               [ 2  1] [ 2 -1] [-2  1] [-2 -1]]
        :let [p [(+ v y) (+ h x)]]
        :when (= 0 (get-in board p))]
    p))

(defn best->worst
  "Using Warnsdorff's Rule determines the priority
  of the several knigt-moves options."
  [board pos]
  (let [op (options board pos)
        sop (map #(vector (count (options board %)) %) op)]
    (vec (map #(last %)(sort sop)))))

(declare check-options)                                      ;; Declare the not yet defined check-options function.

(defn jumps
  " c = counter, n = board size, pos = current position, ipos = initial position.
  If every square of the board has being visitated once, and the final square
  it's at one knight-move from the start, returns board. If c is equal to -1 then
  returns nil. If none of that is true, then calls check-options."
  [board pos c n ipos]
  (cond (and (>= c (* n n)) (isin? (options (board-f n) pos) ipos)) board
        (= c -1) nil
        :else (let [op (best->worst board pos)]
                (check-options board c n op ipos))))

(defn check-options
  "If none options left, then calls 'jumps' with c = -1. Recursivly it checks
  all the options to see if one of them returns the board from 'jumps'."
  [board c n op ipos]
  (if (empty? op)
    (jumps board [] -1 n ipos)
    (let [jps (jumps (assoc-in board (first op) c) (first op) (inc c) n ipos)]
      (if-not (empty? jps)
          jps
          (check-options board c n (rest op) ipos)))))

(defn start
  "Takes a number and return an error if it's an odd one. And if
  is not, returns the board with the solved Closed knight's tour
  in a board of size n."
  [n]
  (if (odd? n)
    (println "Only even numbers for de size of the board")
    (let [pos [(/ n 2) (/ n 2)]]
      (jumps (assoc-in (board-f n) pos 1) pos 2 n pos))))

;; --- Printing The Board --- ;;
(defn digits
  "Returns the digists of a number. Ex: (n = 666) => (6 6 6)."
  [n]
  (->> n str (map (comp read-string str))))

(defn cute-board
  "Prints a board of a visual and conforable way."
  [n]
  (let [b (start n)
        l (inc (count (digits (* n n))))]
    (println "\n")
    (map #(println %)
      (for [x b]
        (apply str (map #(str (apply str
                                     (take (- l (count (digits %)))
                                           (repeat " "))) (str %)) x))))))

;; (cute-board 6)
;; =>
;; 25  6 27 10 23  8
;; 28 17 24  7 30 11
;;  5 26 29 32  9 22
;; 16 33 18  1 12 31
;; 19  4 35 14 21  2
;; 34 15 20  3 36 13

;; (cute-board 14)
;; =>
;; 37  44   5  32  35  70  75  30  73  90  77  28  81  84
;; 6  33  36  69   4  31  72  91  76  29  86  83  78  27
;; 43  38  45  34  71  92 123  74 105 112  89  80  85  82
;; 46   7  68  93 122   3 104 113 124  87 106 111  26  79
;; 39  42  47  96 103 136 121 126 129 114 119  88 107 110
;;  8  67  40 137  94 127   2 135 120 125 130 109 116  25
;; 41  48  95 102  97 138 141 128 167 134 115 118 131 108
;; 66   9  98  61 140 157 168   1 142 165 170 133  24 117
;; 49  62  65 156 101 160 139 166 169 196 143 176 171 132
;; 10  99  60 151  64 155 158 181 164 175 190 187 144  23
;; 59  50  63 100 159 150 161 174 195 182 177 172 189 186
;; 14  11 152  57 154  55 180 163 178 173 188 191  22 145
;; 51  58  13  16  53 162 149  18 183 194 147  20 185 192
;; 12  15  52 153  56  17  54 179 148  19 184 193 146  21