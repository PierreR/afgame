package afgame

import (
	"testing"
)

type fix struct {
	label string
	in    []int
	out   int
}

var fixes = []fix{
	fix{"Simple game", []int{NumPin, 8, 1, 2, 1, 2, 12, 6, 4, 1, NumPin, 8, 2, 3}, 101},
	fix{"Spare game", []int{8, 1, 1, 8, (NumPin - 8), 1, 2, 1, NumPin, 1, 2, 1}, 55},
	fix{"Perfect game", []int{NumPin, NumPin, NumPin, NumPin, NumPin, NumPin, NumPin, NumPin}, 300},
	fix{"Refuse extra shot", playLastFrame(1, 2, 3, 4), 6},
	fix{"Refuse extra shot with 1 strike", playLastFrame(NumPin, 1, 2, 3, 4), 21},
	fix{"Refuse extra shot with 2 strike", playLastFrame(NumPin, NumPin, 1, 2, 3), 33},
	fix{"Last frame with a strike and a spare", playLastFrame(NumPin, 10, (NumPin - 10), 2), 32},
	fix{"Last frame with a strike and a spare at last position", playLastFrame(NumPin, 10, 2, 3), 30},
}

func playLastFrame(shots ...int) []int {
	return append([]int{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, shots...)
}

func TestAll(t *testing.T) {
	for _, fix := range fixes {
		game := New()
		v := game.Shots(fix.in...)
		if v != fix.out {
			t.Errorf("%q %d = %d, want %d.", fix.label, fix.in, v, fix.out)
			t.Log(game.frames)
		}
	}
}

