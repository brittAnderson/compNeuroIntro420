
package main

import (
	"math"

	"gonum.org/v1/plot"
	"gonum.org/v1/plot/plotter"
	"gonum.org/v1/plot/vg"
)

func main() {
	p := plot.New()
	p.Title.Text = "Exponential Decay"
	p.X.Label.Text = "x"
	p.Y.Label.Text = "e^(-x)"

	// Force axes to begin at zero
	p.X.Min = 0
	p.Y.Min = 0
	p.Y.Max = 1

	pts := make(plotter.XYs, 50)
	for i := 0; i < 50; i++ {
		x := float64(i) * 0.1
		pts[i].X = x
		pts[i].Y = math.Exp(-x)
	}

	line, _ := plotter.NewLine(pts)
	p.Add(line)

	p.Save(6*vg.Inch, 4*vg.Inch, "decay.png")
}
