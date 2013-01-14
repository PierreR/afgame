package afgame

// Naming convention (inspired by Kernighan):
//   local func variable : nItems -> number of items
//   other: numItems

import (
	"fmt"
	// "log"
)

const (
	StrikeBonusShot    = 3  // times a strike earns the points of the next following shots
	SpareBonusShot     = 2  // idem for spares
	NumPin             = 15 // nbr of pins to strike
	NumShotNormalFrame = 3  // max nbr of shot inside a frame in a game
	NumShotLastFrame   = 4  // max nbr of shot in the last frame 
	NumFrame           = 5  // nbr of frame in a game
	MaxScore           = 300
)

type Game struct {
	frames   []frame // array used to record the frame played at time t
	numPinUp int // number of pin up in the game at ttime 
}

type frame struct {
	scores    []score
	lastFrame bool
}

// Score records the points earn by one shot.
// Shots happen within a frame, so Score 'd compose in a Frame.
// The last frame only will contain several scores.
// The type 'd add flexibility in case game rule changes in the last frame
type score struct {
	points     int
	bonusCount int
}

func (g *Game) frame() frame {
	return g.frames[len(g.frames)-1]
}

// Create a new African bowling game
func New() *Game {
	frames := make([]frame, 1, NumFrame)
	frames[0] = newFrame(false)
	return &Game{frames, NumPin}
}

func (g Game) Shots(shots ...int) int {
	for _, shot := range shots {
				g.Shot(shot)
	}
	return g.totalScore()
}

// There is 3 steps: accountBonus, recordShot and prepareNextShot
func (g *Game) Shot(shot int) {

	if g.isOver() {
		fmt.Println("Illegal extra shot")
		return
	}
	fmt.Printf("Shot: %d, Pinup: %d. Game frames:  %v \n",  shot, g.numPinUp, g.frames)
	g.accountBonus(shot)
	g.recordShot(shot)
	g.prepareNextShot()
}


// account bonus to previous scores
// the method is meant to be called before the shot is recorded
func (g Game) accountBonus(shot int) {
	for _, frame := range g.frames {
		for j, score := range frame.scores {
			if score.bonusCount > 0 && !frame.lastFrame {
				// of course you cannot update the score local variable directly! 
				frame.scores[j].points += shot
				frame.scores[j].bonusCount--
			}
		}
	}
}

// Record the shot in the current frame:
func (g *Game) recordShot(shot int) {
	// the frame accessor cannot be used on the left because it does not return a pointer but a value
	g.frames[len(g.frames)-1].scores = append(g.frame().scores, newScore(shot, g.numPinUp))
	// remove recorded pins from the board
	g.numPinUp -= shot
}

func (g *Game) prepareNextShot() {
	f := g.frame()
	// are we playing a normal frame, then we might need to init a new one
	if !f.lastFrame && (g.numPinUp == 0 || len(f.scores) == NumShotNormalFrame) {
		last := len(g.frames) == NumFrame-1
		g.frames = append(g.frames, newFrame(last))
		g.numPinUp = NumPin // lift all the pins up
	} else { // else always check if we need to lift the pins back up
		if g.numPinUp == 0 {
			g.numPinUp = NumPin
		}
	}
}

func (g Game) totalScore() (sum int) {
	for _, frame := range g.frames {
		for _, score := range frame.scores {
			sum += score.points
		}
	}
	return
}

func (f frame) sumAllPoints() (sum int) {
	for _, score := range f.scores {
		sum += score.points
	}
	return
}

func newScore(shot int, numPinUp int) score {
	var bonus int
	if numPinUp == NumPin && shot == NumPin {
		bonus = StrikeBonusShot
	} else if numPinUp-shot == 0 {
		bonus = SpareBonusShot
	} else {
		bonus = 0
	}
	return score{shot, bonus}
}

// a bit verbose just to initialize (optimize) the slice to proper default
// Indeed we could just delete this method and create a frame by doing f := []frame{lastFrame:last}
func newFrame(last bool) frame {
	maxShot := NumShotNormalFrame
	lastFrame := false
	if last {
		lastFrame = true
		maxShot = NumShotLastFrame
		/*log.Println("Creating a last frame")*/
	}
	scores := make([]score, 0, maxShot)
	return frame{scores, lastFrame}
}

func (g *Game) isOver() bool {
	if g.frame().lastFrame {
		nShots := len(g.frame().scores)
		/*log.Println(nbOfShots)*/
		if nShots == 1 {
			return false
		}
		if g.frame().sumAllPoints() > NumPin {
			return nShots >= NumShotLastFrame
		}
		return nShots >= NumShotNormalFrame
	}
	return false
}
