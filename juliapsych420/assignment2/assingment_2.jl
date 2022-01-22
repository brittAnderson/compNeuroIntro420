### A Pluto.jl notebook ###
# v0.17.5

using Markdown
using InteractiveUtils

# ╔═╡ 51a247fe-70d0-11ec-1e4f-db0769c894c1
md"
# Assignment 2

## Meta
Author: Parmandeep Chaddha. 

Date: January 8, 2021.  

"

# ╔═╡ 40c79413-9bed-4a64-a66c-6431eb928aea
md"
## Objective
1. Create a basic 4th root function.
"

# ╔═╡ 612536a6-3e16-41d2-9b56-4054c7765b40
fourth_root(x) = x ^ (1/4)

# ╔═╡ fa38774b-7f18-49ec-85b5-5dba93d54de3
# fourth root of 16 is 2
fourth_root(36)

# ╔═╡ 93f4c542-a675-44b0-b58e-fe0085e22ed9
# fourth root of 81 is 3
fourth_root(81)

# ╔═╡ 67979f4d-5e1c-4255-a725-d05ed570653a
md"
2. Create a function that calculates the nth root of a function
"

# ╔═╡ 452df0ab-0592-4756-befe-2523d6c0721e
nth_root(x, n) = x ^ (1/n)

# ╔═╡ 13340caf-88a9-4479-96c3-6e06a1e4cd0c
# The third root of 8 is 2.
nth_root(8, 3)

# ╔═╡ c49f22c5-cf14-4fbc-a6d9-21f66e473f12
# The 1000th root of 1 is 1.
nth_root(1, 1000)

# ╔═╡ b0a9c795-c611-4f02-b7eb-3250c0c59b93
md"
3. Create a 4th root function manually.
"

# ╔═╡ 70b32c5c-c3e7-4df5-846d-4b6805ab7c23
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

# ╔═╡ a2f9ffb2-f368-41c9-8b0d-2fcef458d2fe
# Requires the number for which the fourth root should be found as well as an intial guess.
answer1 = fourth_root_manual(17.0, 3.0)

# ╔═╡ 307dab8b-0f21-44b3-b8af-3be68aca2e38
answer1^4 # Close enough to our original guess of 17!

# ╔═╡ 2a1de8ff-0503-494f-990a-464704b78cc3
md"
4. Create an nth root function manually. Small modification to `fourth_root_manual`.
"

# ╔═╡ 5ae76b88-82e0-4268-80d0-e92c3a6ea1aa
function nth_root_manual(y:: Float64, n:: Int64, guess:: Float64)
	error= 1.e-6
	iterations = 0

	while (error <= abs(y - guess^n)) && (iterations < 100)
		guess = guess + (y - guess^n) / (n*guess^(n-1))
		iterations += 1
	end

	return guess
end

# ╔═╡ 9be0eb1e-ce99-4e11-82a9-c95ddb667e57
# Calculate the fifth root of 100. A good guess is between 2^5 = 32 and 3^5= 243.
# Therefore, a good guess is 2.5
answer2 = nth_root_manual(100., 5, 2.5)

# ╔═╡ 2c454985-768f-42e6-9c74-35240fac9f62
# Lets see if the answer2 to the fifth power is indeed 100.
answer2 ^ 5

# ╔═╡ 908a7d29-126e-46e7-8a60-f19cc013de52
md"
Therefore, the Newton Raphson method works quite well!
"

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

julia_version = "1.7.1"
manifest_format = "2.0"

[deps]
"""

# ╔═╡ Cell order:
# ╟─51a247fe-70d0-11ec-1e4f-db0769c894c1
# ╟─40c79413-9bed-4a64-a66c-6431eb928aea
# ╠═612536a6-3e16-41d2-9b56-4054c7765b40
# ╠═fa38774b-7f18-49ec-85b5-5dba93d54de3
# ╠═93f4c542-a675-44b0-b58e-fe0085e22ed9
# ╟─67979f4d-5e1c-4255-a725-d05ed570653a
# ╠═452df0ab-0592-4756-befe-2523d6c0721e
# ╠═13340caf-88a9-4479-96c3-6e06a1e4cd0c
# ╠═c49f22c5-cf14-4fbc-a6d9-21f66e473f12
# ╟─b0a9c795-c611-4f02-b7eb-3250c0c59b93
# ╠═70b32c5c-c3e7-4df5-846d-4b6805ab7c23
# ╠═a2f9ffb2-f368-41c9-8b0d-2fcef458d2fe
# ╠═307dab8b-0f21-44b3-b8af-3be68aca2e38
# ╟─2a1de8ff-0503-494f-990a-464704b78cc3
# ╠═5ae76b88-82e0-4268-80d0-e92c3a6ea1aa
# ╠═9be0eb1e-ce99-4e11-82a9-c95ddb667e57
# ╠═2c454985-768f-42e6-9c74-35240fac9f62
# ╟─908a7d29-126e-46e7-8a60-f19cc013de52
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
