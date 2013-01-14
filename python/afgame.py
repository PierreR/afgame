__author__ = 'pradermecker'

"""
  To use this script, use the score(@input) method of the Game class
  The method returns the score of the Game.

  A detail result with score per frame is available through the attribute "frames" of a game instance.

  Last frame:
  The main tricky part of the computation occurs in the last frame.
  This is because extra shots are allowed to account for bonus points of the strikes and spares performed.
  This script considers that these extra shots only serves to count the bonus of theses strikes or spares.
  Here is what happen when a spare have to be accounted for a previous strike:
  input : [strike,10,3,2] -> the score is :  15 + 15 (not 15+10+3+15).
  The maximum score is [strike, strike, strike, strike] which gives a score of 60.
  See the test cases for more details.

  A strike or a spare always earns the points of all pins down (15), so we only have to sum all shots to account for these
  [5, spare] -> 5 + 10 + bonus
 
 """
STRIKE_BONUS_SHOT = 3 # a strike earns the points of the next STRIKE_BONUS_SHOT shots
SPARE_BONUS_SHOT = 2 # a spare earns the points of the next SPARE_BONUS_SHOT shots
ALL_PINS = 15 # there is 15 bowling pins to strike
NORMAL_FRAME_SHOTS = 3 #  a frame is composed of NORMAL_FRAME_SHOTS
MAX_LAST_FRAME_SHOTS = 4 #  except the last frame which can be made of MAX_LAST_FRAME_SHOTS
MAX_FRAME_COUNT = 5 #  A game is composed of 5 frames
MAX_SCORE = 300 #  the max score is 300

# a strike earns 15 points but you don't need the constant because you will get the points of all pins down for a strike anyway
#  a spare earns 15 points but you don't need the constant because we always sum all the pins down

class Board:
    """
    The board represents the state of the pins plate in a game.
    """
    def __init__(self):
        self.pins = ALL_PINS

    def update(self, shot):
        self.pins -= shot

    def has_all_pins_up(self):
        return self.pins == ALL_PINS

    def has_all_pins_down(self):
        return self.pins == 0

    def reset(self):
        self.pins = ALL_PINS


class Score:
    """
    Score records the points that earn a shot
    """
    def __init__(self, shot, bonus=0):
        self.points = shot
        self.bonus = bonus

    def update(self, shot):
        if self.bonus > 0:
            self.points += shot
        self.bonus -= 1


class Game:
    """
    The game is responsible for driving the process. 
    It receives shots, create new frames, update scores, ...
    """
    def __init__(self):
        self.frames = [Frame()]
        self.board = Board()

    def __calcBonus(self, shot):
        "Calculate the bonus knowing the board has not been updated yet"
        if self.board.has_all_pins_up() and shot == ALL_PINS:
            return STRIKE_BONUS_SHOT
        elif self.board.pins - shot == 0:
            return SPARE_BONUS_SHOT
        else:
            return 0

    def __update_scores(self, shot):
        for frame in self.frames: frame.updateScores(shot)

    def __is_end_of_frame(self):
        "This one assumes the board has already been updated by the shot"
        if len(self.frames) < MAX_FRAME_COUNT:
            if self.board.has_all_pins_down() or len(self.frames[-1].scores) == NORMAL_FRAME_SHOTS:
                return True
        else:
            return False

    def shot(self, *args):
        for arg in args:
            self.__update_scores(arg)
            current_frame = self.frames[-1]
            if current_frame.is_game_over():
                print("Illegal shot")
                continue
            current_frame.shot(Score(arg, self.__calcBonus(arg)))
            # Update the board after the creation of the Score not before
            self.board.update(arg)
            if self.__is_end_of_frame():
                frame = Frame() if len(self.frames) < MAX_FRAME_COUNT - 1  else LastFrame()
                self.frames.append(frame)
                self.board.reset()
            else:
                if self.board.has_all_pins_down(): self.board.reset()
        return self.score()

    def score(self):
        scores = [frame.score() for frame in self.frames]
        print(scores)
        score = sum(scores)
        return score if score <= MAX_SCORE else MAX_SCORE


class Frame:
    def __init__(self):
        self.scores = []

    def is_game_over(self):
        return False

    def updateScores(self, shot):
        for score in self.scores:
            score.update(shot)

    def shot(self, score):
        self.scores.append(score)

    def score(self):
        return sum([score.points for score  in self.scores])


class LastFrame(Frame):
    """
    Amazingly enough, if we don't update any scores in the last frame,
    it gets quite simple: just sum up the shots (strike, spare or normal)
    and let the game know when to refuse extra shots
    """
    def updateScores(self, shot):
        return

    def is_game_over(self):
        nbr_of_shots = len(self.scores) + 1
        if nbr_of_shots == 1: return False
        if self.score() > ALL_PINS:
            return nbr_of_shots > MAX_LAST_FRAME_SHOTS
        else:
            return nbr_of_shots > NORMAL_FRAME_SHOTS
