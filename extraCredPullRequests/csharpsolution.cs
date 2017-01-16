using System;

namespace Assignments
{
    public class Launcher
    {
        public static void Main(string[] args)
        {
            Console.Out.WriteLine("First guess of -2");
            var finalVal = GuessZero(-2);
            Console.Out.WriteLine($"Final guessed value from original guess of -2: {finalVal}");

            Console.Out.WriteLine(string.Empty);
            Console.Out.WriteLine("Second guess of 2");
            finalVal = GuessZero(2);
            Console.Out.WriteLine($"Final guessed value from original guess of 2: {finalVal}");

            Console.In.Read();
        }

        /// <summary>
        /// Guesses a zero value for a given starting guess
        /// </summary>
        /// <param name="guess">Tee startin guess</param>
        /// <returns>The final guess</returns>
        private static double GuessZero(double guess)
        {
            double delta = 1.0;
            double val = guess;

            // Keep iterating until our delta is less than 0.001
            while (delta > 0.001)
            {
                // Calculate our new cubed plus root value
                var result = CubedPlusRoot(val);
                delta = Math.Abs(val - result);

                Console.Out.WriteLine($"A value of {val} has been guessed with a delta of {delta}");
                val = result;
            }

            return val;
        }

        /// <summary>
        /// Returns the calculated value of the cubed plus the squared
        /// </summary>
        /// <param name="x">The original value</param>
        /// <returns>x^3 + x^2</returns>
        private static double CubedPlusSquared(double x)
        {
            return Math.Pow(x, 3) + Math.Pow(x, 2);
        }

        /// <summary>
        /// Returns the value calculated with the derivative of the cubed plus the squared
        /// values of the original value
        /// </summary>
        /// <param name="x">The original value</param>
        /// <returns>3x^2 + 2x</returns>
        private static double CubedPlusSquaredDeriv(double x)
        {
            return 3 * Math.Pow(x, 2) + 2 * x;
        }

        /// <summary>
        /// Attempts a guess at a zero value using Newtons methods
        /// </summary>
        /// <param name="value">The starting guess value</param>
        /// <returns>The new guess: x = x` - f(x)/f'(x)</returns>
        private static double CubedPlusRoot(double value)
        {
            return value - CubedPlusSquared(value) / CubedPlusSquaredDeriv(value);
        }
    }
}
