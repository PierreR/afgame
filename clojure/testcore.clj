(ns afgame.test.core
  (:use [afgame.core])
  (:use [midje.sweet])
  (:use [clojure.test]))

(fact "Play simple game"
      (play [1]) => 1
      (play [15]) => 15
      (play [1 2 3 1 2 3 1 2 3 1 2 3 1 2 3]) => (+ 6 6 6 6 6 ))

(fact "Extra shots don't count"
      (play [1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 4]) => (+ 6 6 6 6 6 ))

(fact "Play a strike"
      (play [15 15 1]) => (+ 15 15 1 15 1 1)
      (play [15 15 15 1 2 3]) => (+ 15 31 15 18 15 6 6)
      (play [15 1 2 3]) => (+ 15 1 2 3 1 2 3))

(fact "Play a strike and  a spare"
      (play [15 1 2 3 10 5 1 2 3]) => (+ 15 1 2 3 6 10 5 1 2 6)
      (play [10 4 1 1 2 3]) => (+ 10 4 1 1 2 1 2 3))

(fact "Play the last frame without spare or strike"
      (play [15 15 15 15 1 2 3]) => (+ 60 15 31 15 18 15 6 6) 
      (play [15 15 15 15 1 2 3 4]) => (+ 60 15 31 15 18 15 6 6)) ; same result as above the game is over before the last shot

(fact "Play the last frame with strike"
      (play [15 15 15 15 15 15 15 15]) => (+ 60 60 60 60 15 45 15 30 15 15 15)  
      (play [15 15 15 15, 15 15 15 1]) => (+ 60 60 60 60, 46 31 16 1))

(fact "Play the last frame with a spare"
      (play [15 15 15 15, 10 5 1 2]) => (+ 60 55 45 31, 15 3 1 2)
      (play [15 15 15 15, 15 10 5 1]) => (+ 60 60 55 45, 31 15 1 1)) 