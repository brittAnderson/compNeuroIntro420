### A Pluto.jl notebook ###
# v0.17.5

using Markdown
using InteractiveUtils

# ╔═╡ bf1b1338-098b-43b3-9674-6f188f26183c
using Pkg

# ╔═╡ de093f94-8b25-491e-b5f9-46a36f9e05c2
Pkg.activate("Project.toml")

# ╔═╡ b6bcde7e-2b46-4652-96e6-2bfce7c55099
using VegaLite

# ╔═╡ b0677a57-c920-4a8d-881c-8f37be8c65fe
using Random

# ╔═╡ 584753ec-1c80-4403-af03-7d136d7fda4a
using DataFrames

# ╔═╡ 07e46038-f6fc-4772-982b-bc563252b4fa
md"# Pluto Interactive Notebook
## Meta
Author: Parmandeep Chaddha.

Date: January 7, 2021
"

# ╔═╡ 96fda284-8022-4941-8f21-e346b83f182e
Pkg.status()

# ╔═╡ da62026e-9dc2-4641-a53b-d801d0a56928
md"
## Some Beginner Code
"

# ╔═╡ 1c06ec5f-a85e-4430-b964-477bc8023384
1 + 1

# ╔═╡ aaaebc17-860a-48ff-a14d-5a505e95e5ac
vals = rand(1000); 

# ╔═╡ f00047b6-4a26-4c23-9481-4c682574b6a9
df = DataFrame(random_values=vals)

# ╔═╡ Cell order:
# ╟─07e46038-f6fc-4772-982b-bc563252b4fa
# ╠═bf1b1338-098b-43b3-9674-6f188f26183c
# ╠═de093f94-8b25-491e-b5f9-46a36f9e05c2
# ╠═96fda284-8022-4941-8f21-e346b83f182e
# ╟─da62026e-9dc2-4641-a53b-d801d0a56928
# ╠═1c06ec5f-a85e-4430-b964-477bc8023384
# ╠═b6bcde7e-2b46-4652-96e6-2bfce7c55099
# ╠═b0677a57-c920-4a8d-881c-8f37be8c65fe
# ╠═584753ec-1c80-4403-af03-7d136d7fda4a
# ╠═aaaebc17-860a-48ff-a14d-5a505e95e5ac
# ╠═f00047b6-4a26-4c23-9481-4c682574b6a9
