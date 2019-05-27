package main

import (
  "bufio"
  "fmt"
  "os"
  "math"
  "strconv"
  "strings"
  "log"
)

func square(x float64) float64 {
	return math.Pow(x, 2)
}

func derivSquare(x float64) float64{
	return (2 * x)
}

func squareRoot(n float64, initGuess ... float64) float64 {
	guess := 5.0
	if initGuess != nil {
		guess = initGuess[0]
	}
	error := 10000.0
	tolerance := 0.001

	for {
		if (math.Abs(error) <= tolerance) {
			break
		}
		error = square(guess) - n
		guess = ((-1 * error) / derivSquare(guess)) + guess
	}

	return guess
}

func main() {

	reader := bufio.NewReader(os.Stdin)

	for {
		fmt.Println("Square root of ?")
		str_num, _ := reader.ReadString('\n')
		str_num = strings.TrimRight(str_num, "\n")
		int_num, err := strconv.ParseFloat(str_num, 64)
		if err != nil {
			log.Fatal(err)
		}

		sqr_root := squareRoot(int_num)

		fmt.Printf("Answer is: %g\n", sqr_root)
  	}
  }
