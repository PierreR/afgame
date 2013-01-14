(ns afgame.core)

(def all-pins 15)
(def nframes 5)
(def max-shots-in-last-frame 4)
(def max-shots-in-frame 3)

(defn create-board 
  "Board is a vector of frames. A frame is a map with keys :shots ane :score"
  []
  (vec (repeat nframes {:shots [] :score 0})))

(defn game-over? 
  "In the last frame, return true if the max of shots has been reached."
  [frame]
  (when (:last frame) 
    (let [shots (:shots frame)]
      ; check if a strike or spare has been shot
      (if (>= (reduce + shots) all-pins)
        ; if so extra-shots are allowed
        (= max-shots-in-last-frame (count shots))
        (= max-shots-in-frame (count shots))))))

(defn- calc-score
  "Return the score gained by the shot (not the score of the current frame !).  
  The challenge here is to have an algorithm that works with both normal and last frame.
  In case of a spare or a normal shot we can use the sum of fshots removing all strikes in order to detect spare."
  [shot fshots rshots]
  (let [sum (reduce #(+ %1 (if (= %2 15) 0 %2)) 0 fshots)]
    (cond 
      (= shot all-pins) ; strike
      (reduce + (conj (take 3 rshots) shot))
      (= sum all-pins)  ; spare
      (reduce + (conj (take 2 rshots) shot)) ;  
      :else ; plain shot
      shot)))

(defn- calc-game
 "This reduction function uses a map as a convenient data structure to pass the state of the game at each iteration.
 The map has 3 fields : a board (collection of frames), the remaining shots to be played and a frame index (fidx) that points to the current frame." 
  [{board :board rshots :rshots fidx :fidx} shot]
  (let [frame (nth board fidx nil)] ; the current frame 
    (if (game-over? frame)
      {:board board} ; game over return the board
      (let [fshots (:shots frame)
            frame-completed? (when-not (:last frame) ; the last frame is never completed (see game-over?)
                               (or (= all-pins (reduce + fshots)) ;  completed if the sum of all shots within a frame is all the pins
                                   (= max-shots-in-frame (count fshots)))) ; or if max-shots has been played
            [ updated-fidx , updated-fshots ] (if frame-completed?
                                                [ (inc fidx) , (vector shot) ]
                                                [ fidx       , (conj fshots shot) ])
            ; sum-up the score of the current frame with the score gained by the shot
            score (+  (:score (nth board updated-fidx)) (calc-score shot updated-fshots rshots))]
        {:board (assoc board updated-fidx {:shots updated-fshots :score score :last (= updated-fidx (dec nframes)) }) :rshots (rest rshots) :fidx updated-fidx}))))

(defn get-score 
  "Get the score of the game."
  [{board :board}] ; expect a map (the game) with a board field.
  (reduce + (map :score board)))

(defn ^int play
  "Return the score for a list of shots each representing the number of pin knocked down.
  In case the game is played in timely separated steps, the application should record the shots (not the game) in a DB
  and always replay the whole set of shots to return the score."
  [shots] 
  (assert (not-any? #(> % all-pins) shots) (str "A shot should not be over " all-pins))
  (get-score (reduce calc-game {:board (create-board) :rshots (rest shots) :fidx 0} shots)))