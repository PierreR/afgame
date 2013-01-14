e this script, use the score(@input) method
 * The method returns void.
 * To get the result, use score() that return the total score
 * A detail result with score per frame is available in the global variable "frames".
 * Every time score(@input) is called, the game is reset: previous game results are not kept.
 *
 * Last frame:
 * The main tricky part of the computation occurs in the last frame.
 * This is because extra shots are allowed to account for bonus points of the strikes and spares performed. 
 * This script considers that these extra shots only serves to count the bonus of theses strikes or spares.
 * Here is what happen when a spare have to be accounted for a previous strike:
 * input : [strike,10,3,2] -> the score is :  15 + 15 (not 15+10+3+15). 
 * The maximum score is [strike, strike, strike, strike] which gives a score of 60.
 * See the test cases for more details.
 * 
 * 
 * */


/**
 * There is no module in js:
 *  if we don't want to include a ext lib we have to put tests and codes together in the same file
 */


/**
 * UTILS
 */

var newObj = function(o) {
  function F () {};
  F.prototype = o;
  return new F()
}

/**
 * Configuration CONSTANT (no primitive constant in JS)
 */
var STRIKE_BONUS_SHOT = 3, // a strike earns the points of the next STRIKE_BONUS_SHOT shots
    SPARE_BONUS_SHOT = 2, // a spare earns the points of the next SPARE_BONUS_SHOT shots
    STRIKE_SCORE = 15, // a strike earns STRIKE_SCORE points
    SPARE_SCORE = 15, // a spare earns SPARE_SCORE points
    ALL_PINS = 15, // there is 15 bowling pins to strike
    NORMAL_FRAME_SHOTS = 3, // a frame is composed of NORMAL_FRAME_SHOTS
    MAX_LAST_FRAME_SHOTS = 4, // except the last frame which can be made of MAX_LAST_FRAME_SHOTS
    MAX_FRAME_COUNT = 5, // A game is composed of 5 frames
    MAX_SCORE = 300; // the max score is 300

/**
 * The Game module
 */
var Game = (function() {

  var module = {};

  var newScore = function (shot, bonus) {
    return {
      update: function(shot) {
        if (this.bonus > 0) {
          this.points += shot;
          this.bonus --;
        }
      },
      points: shot,
      bonus: bonus
    }
  }

  /**
   * Return an object representing a frame
   */
  var newFrame = function(seq) {
    return { // Return the object with the public methods
      scores: [],
      isGameOver: function () {
        return false;
      }, 
      updateScores: function(shot) {
        this.scores.forEach(function(score) { score.update(shot); });
      },
      shot: function(score) {
        this.scores.push(score);
      },
      getNbOfShots: function() {
        return this.scores.length;
      },      
      getScore: function() {
        var sum = 0;
        this.scores.forEach(
            function(score) {
              sum += score.points;
            }
        );
        return sum;
      }   
    };
  };

  var newLastFrame = function(seq) {
    var ext = newObj(newFrame());
    ext.updateScores = function(shot) {
      return;
    };
    ext.isGameOver = function () {
      var nbOfShots = this.getNbOfShots() + 1; // plus the current shot
      if (nbOfShots == 1) return false;
      if (this.getScore() > ALL_PINS) {
        return nbOfShots > MAX_LAST_FRAME_SHOTS;
      } else {
        return nbOfShots > NORMAL_FRAME_SHOTS;
      }
    }; 
    return ext;
  };

 module.New = function() {

    var remainingPinsOnBoard = ALL_PINS;

    function updateScores(frames, shot) {
      frames.forEach(function(frame) { frame.updateScores(shot);});
    };
    
    function hasAllPinsUp() {
      return remainingPinsOnBoard === ALL_PINS;
    };
    
    function hasAllPinsDown() {
      return remainingPinsOnBoard === 0;
    };
    
    function calcBonus(shot) {
      if (hasAllPinsUp() && shot === ALL_PINS) {
        return STRIKE_BONUS_SHOT;
      } else if (remainingPinsOnBoard - shot === 0) {
        return SPARE_BONUS_SHOT;
      } else {
        return 0;
      }
    };
    
    function isEndOfFrame(frames) {
      if (frames.length < MAX_FRAME_COUNT) {
        if (hasAllPinsDown() || frames[frames.length - 1].getNbOfShots() === NORMAL_FRAME_SHOTS) {
          return true;
        }
      } else {
          return false;
      }
    }; 
      
    return {
      frames : [newFrame()], // Array containing all played frames including the current one.
      shot: function() {
        var frames = this.frames;
        for(var i = 0; i < arguments.length; i++) {
          var shot = arguments[i],
              currentFrame = frames[frames.length -1];
          updateScores(frames, shot);
          if (currentFrame.isGameOver()) {
            console.info("Illegal shot");
            continue;
          }
          currentFrame.shot(newScore(shot, calcBonus(shot)));
          remainingPinsOnBoard -= shot;
          if (isEndOfFrame(frames)) {
            var f = (frames.length < MAX_FRAME_COUNT - 1) ? newFrame() : newLastFrame();
            frames.push(f);
            remainingPinsOnBoard = ALL_PINS;
          } else {
            if (hasAllPinsDown()) {
              remainingPinsOnBoard = ALL_PINS;
            }
          }
        }
        return this.getTotalScore();
      },
      getTotalScore: function() {
        var sum = 0;
        this.frames.forEach(
            function(frame) {
              sum += frame.getScore();
        });
        return sum;
      }
    };
  }
 // Provide a special last frame only game for test  purpose
 module.NewLastFrame = function () {
   var game = module.New();
   game.frames = game.frames.concat([newFrame(), newFrame(), newFrame(), newLastFrame()]);
   return game;
 } 
 
 return module;
}());



// TEST CASES
/*
 * The last shot of all the following tests should be refused
 */

function assert(expected, total) {
  if (total === expected) {
    console.log("OK, score is " + expected);
  } else {
    console.warn("KO: expected " + expected + " but was " + total);
  }
}


function testRefuseExtraIllegalShots() {

  var total;
  
  total = Game.NewLastFrame().shot(1, 2, 3, 4);
  assert(6, total);
  total = Game.NewLastFrame().shot(ALL_PINS, 1, 2, 3, 4);
  assert(21, total);
  total = Game.NewLastFrame().shot(ALL_PINS, ALL_PINS , 1, 2, 3);
  assert(33, total);
}

function allTests() {
  var total;
  total = Game.New().shot(ALL_PINS, 8, 1, 2, 1, 2, (ALL_PINS-2-1), 6, 4, 1, ALL_PINS, 8, 2, 3);
  assert(101, total);
  total = Game.New().shot(8, 1, 1, 8, (ALL_PINS - 8) , 1, 2, 1, ALL_PINS, 1, 2, 1);
  assert(55, total);
  total = Game.New().shot(ALL_PINS, ALL_PINS, ALL_PINS, ALL_PINS, 1, 2, 2);
  assert(164, total);
  // Perfect game
  total = Game.New().shot(ALL_PINS, ALL_PINS, ALL_PINS, ALL_PINS, ALL_PINS, ALL_PINS, ALL_PINS, ALL_PINS);
  assert(300, total);
  // last frame with one spare
  total = Game.NewLastFrame().shot(8, (ALL_PINS-8), 1, 2);
  assert(18, total);
  // last frame with a strike and a spare
  total = Game.NewLastFrame().shot(ALL_PINS, 10, (ALL_PINS-10), 2);
  assert(32, total);
  // last frame with a strike and a spare at last position
  total = Game.New().shot(ALL_PINS, ALL_PINS, ALL_PINS, ALL_PINS, ALL_PINS, 10, 2, 3);
  assert(247, total);
  testRefuseExtraIllegalShots();
}
