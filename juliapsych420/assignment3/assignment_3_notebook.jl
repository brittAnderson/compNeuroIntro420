### A Pluto.jl notebook ###
# v0.17.5

using Markdown
using InteractiveUtils

# ╔═╡ e2ad439e-65ba-46a8-852b-77b6d997cebd
include("./assignment3/DampedOscillatorClass.jl")

# ╔═╡ 7e4e17d1-1586-4d7c-8c38-56234b501329
md"
# Assignment 3

### Meta
Author: **Parmandeep Chaddha**
Date: **Jan 22, 2022**

### Objective
Imports the DampedOscillator structure and related functions from `./DampedOscillatorClass.jl`. Calculates and plots oscillators with various dampening constants.
"

# ╔═╡ a66aa8fe-ce48-46cc-8a09-d2a5e6f46112
osc = Main.DampedOscillatorClass.initializeOscillator(10.0, -2.0, 3.0, 0.4, 0.1)

# ╔═╡ 094cad36-72f1-4563-8f3c-3ef6d0d5eb4c
runtime = 20.0 # seconds

# ╔═╡ 8c3d978d-2a25-44ec-8cd3-4b55e7b89c4e
releaseSpring(osc, runtime)

# ╔═╡ 5ca91c5a-42ba-4d21-bdad-ab236ce49dc4
plotSpring(osc, "position")

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
# ╟─7e4e17d1-1586-4d7c-8c38-56234b501329
# ╠═e2ad439e-65ba-46a8-852b-77b6d997cebd
# ╠═a66aa8fe-ce48-46cc-8a09-d2a5e6f46112
# ╠═094cad36-72f1-4563-8f3c-3ef6d0d5eb4c
# ╠═8c3d978d-2a25-44ec-8cd3-4b55e7b89c4e
# ╠═5ca91c5a-42ba-4d21-bdad-ab236ce49dc4
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
