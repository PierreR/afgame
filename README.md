Description
===========

Implementation in several languages of the african bowling game.

The sole purpose is to try out different languages with a basic use cases.


Specifications
==============

Recent research in the field of African archeology have uncovered the rules of an ancient game whose
rules here follow (strangely similar to bowling!)

* Players must throw a piece of wood in the direction of 15 bowling pins and try to shoot up. For
each player, the game breaks down into 5 frames being composed of 3 shots. After each
frame, the pins are straightened.
* In one frame, if all 3 shots failed to shoot down all the pins, the pins are counted for each
launch.
* If all the pins are shot with the first launch of a frame, it is a strike. If all pins are knocked down,
after the 2nd or 3rd run, it is a spare.
* A spare earns 15 points plus the points for the next 2 shots, or maximum 45 if the following 2
shots are strikes.
* A strike earns 15 points plus the points for the next 3 shots, or maximum 60 if the following 3
shots are strikes.
* During a strike or a spare in last frame, the player can make extra shots to allow the calculation
of points in the last frame. The fifth frame may be composed of up to four shots.

Optional rules
==============

* What we mean by earning the points for the next 2 or 3 shots is not really explicit.
  In general, the current implementations has choosen to account the number of pins knocked down by a shot,
  not the score of the shot (for instance, if the next shot is a strike, 15 is accounted, not 15 + the next
  two shots).
* The maximum score is 300, also known as a perfect game.


Implementation History
======================

1. Javascript
2. Python
3. Google Go
4. Clojure
5. Haskell
