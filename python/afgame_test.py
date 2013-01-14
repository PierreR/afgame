__author__ = 'pradermecker'

import unittest
from afgame import *

def new_last_frame_game():
    game = Game()
    game.frames = [Frame(), Frame(), Frame(), Frame(), LastFrame()]
    return game

class TestGames(unittest.TestCase):
    def testRefuseExtraIllegalShots(self):
        self.assertEqual(new_last_frame_game().shot(1, 2, 3, 4), 6)
        self.assertEqual(new_last_frame_game().shot(ALL_PINS, 1, 2, 3, 4), 21)
        self.assertEqual(new_last_frame_game().shot(ALL_PINS, ALL_PINS, 1, 2, 3), 33)

    def testGames(self):
        self.assertEqual(Game().shot(ALL_PINS, 8, 1, 2, 1, 2, (ALL_PINS - 2 - 1), 6, 4, 1, ALL_PINS, 8, 2, 3), 101)
        self.assertEqual(Game().shot(8, 1, 1, 8, (ALL_PINS - 8), 1, 2, 1, ALL_PINS, 1, 2, 1), 55)
        self.assertEqual(Game().shot(ALL_PINS, ALL_PINS, ALL_PINS, ALL_PINS, 1, 2, 2), 164)

    def testPerfectGame(self):
        self.assertEqual(Game().shot(ALL_PINS, ALL_PINS, ALL_PINS, ALL_PINS, ALL_PINS, ALL_PINS, ALL_PINS, ALL_PINS),
                         300)

    def testLastFrame(self):
        # With one spare
        self.assertEqual(new_last_frame_game().shot(8, (ALL_PINS - 8), 1, 2), 18)
        # With a strike and a spare
        self.assertEqual(new_last_frame_game().shot(ALL_PINS, 10, ALL_PINS - 10, 2), 32)
        # With a strike and a spare at last position
        self.assertEqual(Game().shot(ALL_PINS, ALL_PINS, ALL_PINS, ALL_PINS, ALL_PINS, 10, 2, 3), 247)

if __name__ == '__main__':
    unittest.main()

  
