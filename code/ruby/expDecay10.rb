require "numo/gnuplot"

# Generate x and y values for exponential decay
x = (0..100).map { |i| i * 0.1 }
y = x.map { |v| Math.exp(-0.5 * v) }

# print x and y values for exponential decay
x.zip(y).each { |x_value, y_value| puts "#{x_value}: #{y_value}" }

# Plot the exponential graph
Numo.gnuplot do
  set terminal: "pngcairo", size: "800,600"; set output: "expdecay.png"
  set title: "Exponential Decay"; set xlabel: "x"; set ylabel: "y"
  plot x, y, with: "lines", title: "y = e^(-0.5x)"
end

