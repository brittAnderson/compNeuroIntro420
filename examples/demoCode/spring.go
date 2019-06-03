package main

import (
  "math"
  "image/color"

  "gonum.org/v1/plot"
  "gonum.org/v1/plot/plotter"
  "gonum.org/v1/plot/vg"

)

// This function models the behaviour of the spring. It returns 2 arrays, locations[i] is the position
// of the spring at time times[i]
// The plotting function takes a plotter.XY type, so we could have made this function return this
// type instead of 2 arrays. But let's do it like this to improve readability for now.
func modelSpring(initVelocity, initLocation, p, c, stepSize, beginTime, endTime float64) (locations, times []float64) {
    // Initialize the variables that will change as we loop through time
    location := initLocation
    velocity := initVelocity
    time := beginTime
    acceleration := 0.0
    distance := 0.0

    // The break condition for this loop has to be >= and not strict equality due to float precision
    // behaviour
    for {
        if (time >= endTime) {
            break
        }

        acceleration = (-p * location) - (c * velocity)
        velocity += acceleration
        distance = velocity * stepSize
        location += distance

        locations = append(locations, location)
        times = append(times, time)

        time += stepSize
    }

    return locations, times
}

// Helper function to find the max and min values for the x and y axis so that we can force the
// plot to show more than these max and mins
func findAxisMinMax(yValues, xValues []float64) (xMax, xMin, yMax, yMin float64) {
    // Initialization
    xMax = xValues[0]
    xMin = xValues[0]
    yMax = yValues[0]
    yMin = yValues[0]

    for i := range xValues {
        if xValues[i] > xMax {
            xMax = xValues[i]
        }
        if xValues[i] < xMin {
            xMin = xValues[i]
        }
    }

    for j := range yValues {
        if yValues[j] > yMax {
            yMax = yValues[j]
        }
        if yValues[j] < yMin {
            yMin = yValues[j]
        }
    }

    // Round floats to nearest ints
    xMax = math.Round(xMax)
    xMin = math.Round(xMin)
    yMax = math.Round(yMax)
    yMin = math.Round(yMin)

    return xMax, xMin, yMax, yMin
}

// Helper function to convert our separate locations and times arrays into plotter.XY types for
// the plotting module
func buildPlotPoints(locations, times []float64) plotter.XYs {
    pts := make(plotter.XYs, len(locations))
    // Loop through each position in length of pts
    for i := range pts {
        pts[i].X = times[i]
        pts[i].Y = locations[i]
	}
	return pts
}

// Helper function to build the plot, given locations and times
func drawGraph(frictionless_locations, frictionless_times, dampened_locations, dampened_times []float64) {
    // Declare a new plot
    p, err := plot.New()
	if err != nil {
		panic(err)
	}

    p.Title.Text = "Position of Dampened and Frictionless Springs through Time"
	p.X.Label.Text = "Time"
	p.Y.Label.Text = "Location"

    _, _, yMax, yMin := findAxisMinMax(frictionless_locations, frictionless_times)
    p.Y.Min = yMin - 2.0
	p.Y.Max = yMax + 1.0
    p.Add(plotter.NewGrid())

    // Add frictionless line to our plot
    frictionless_points := buildPlotPoints(frictionless_locations, frictionless_times)

    f_line, err := plotter.NewLine(frictionless_points)
	if err != nil {
		panic(err)
	}

	f_line.LineStyle.Width = vg.Points(1)
    f_line.LineStyle.Dashes = []vg.Length{vg.Points(1), vg.Points(1)}
	f_line.LineStyle.Color = color.RGBA{B: 128, G: 128, A: 255}

	p.Add(f_line)
    p.Legend.Add("Frictionless Spring", f_line)

    // Add dampened line to our plot
    dampened_points := buildPlotPoints(dampened_locations, dampened_times)

    d_line, err := plotter.NewLine(dampened_points)
    if err != nil {
        panic(err)
    }

    d_line.LineStyle.Width = vg.Points(1)
    d_line.LineStyle.Dashes = []vg.Length{vg.Points(1), vg.Points(1)}
    d_line.LineStyle.Color = color.RGBA{R: 128, A: 255}

    p.Add(d_line)
	p.Legend.Add("Dampened Spring", d_line)

	p.Legend.Top = true

    // Save the graph as a png in the current working dir
    err = p.Save(8*vg.Inch, 8*vg.Inch, "spring_positions.png")
    if err != nil {
		panic(err)
	}
}

func main() {
    // For a function like this one that takes 7 arguments, one drawback of Go is that it does not
    // support named arguments. As such, I've declared a bunch of variables here so it's easier to
    // read and alter as desired. (Note: In the future, this could potentially be done with structs)
    initVelocity := 0.0    // Initial Velocity
    initLocation := 2.0    // Initial position of spring
    p := 5.0              // Constant
    frictionless_c := 0.1  // Dampening Constant
    dampened_c := 0.0      // Dampening Constant
    stepSize := 0.01       // How big of a delta between t_n and t_n+1
    beginTime := 0.0       // Starting Time
    endTime := 2.0         // End Time

    frictionless_locations, frictionless_times := modelSpring(initVelocity, initLocation, p, frictionless_c, stepSize, beginTime, endTime)
    dampened_locations, dampened_times := modelSpring(initVelocity, initLocation, p, dampened_c, stepSize, beginTime, endTime)
    drawGraph(frictionless_locations, frictionless_times, dampened_locations, dampened_times)

}
