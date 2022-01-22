using Markdown
using InteractiveUtils

# A function that calculates the 4th root of x
fourth_root(x) = x ^ (1/4)

# A function that calculates the nth root of x
nth_root(x, n) = x ^ (1/n)

# A 4th root approximation using Newton Raphson
function fourth_root_manual(y:: Float64, guess:: Float64)
	# let the number we are finding be x.
	# we know y = x*x*x*x
	# 0 = x*x*x*x - y
	error= 1.e-6
	iterations = 0

	while (error <= abs(y - guess^4)) && (iterations < 100)
		guess = guess + (y - guess^4) / (4*guess^3)
		iterations += 1
	end

	return guess
end

# An nth root approximation using Newton Raphson. Small modification to `fourth_root_manual`.
function nth_root_manual(y:: Float64, n:: Int64, guess:: Float64)
	error= 1.e-6
	iterations = 0

	while (error <= abs(y - guess^n)) && (iterations < 100)
		guess = guess + (y - guess^n) / (n*guess^(n-1))
		iterations += 1
	end

	return guess
end

number1 = 36.
answer1 = fourth_root(number1)
answer2 = nth_root(number1, 5)
answer3 = fourth_root_manual(number1, 1.)
answer4 = nth_root_manual(number1, 5, 1.)

print("The fourth root of $(number1) is $(answer1).")
print("The fifth root of $(number1) is $(answer2).")
print("The fourth root estimation of $(number1) using Newton Raphson is $(answer3).")
print("The fifth root estimation of $(number1) using Newton Raphson is $(answer4).")

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

julia_version = "1.7.1"
manifest_format = "2.0"

[deps]
"""