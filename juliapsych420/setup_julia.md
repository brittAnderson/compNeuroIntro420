# Julia and Python for PSYCH 420

## Meta  
Date: Jan 7, 2021  
Author: Parmandeep Chaddha
Last Update: Jan 22, 2022

## Julia Set Up

### 0. Download Julia
1. Download the default Julia installation at: [Julia Download Link](https://julialang.org/downloads/)

### 1. Linking Julia to Terminal
1. Open up the `~.zshrc` file and add the Julia server exectuable path to it as the following:
`alias julia="exec '/Applications/Julia-1.7.app/Contents/Resources/julia/bin/julia'"` 

2. Add the same path to VS Code, if using VS Code.

### 2. Creating a Julia Environment
1. Go to the parent directory of where the environment should be created.
`~./parent`

2. Run the command:
`julia -e'using Pkg;Pkg.generate("name_of_env")'`

3. Change directory into the newly created environment.
`cd name_of_env`

4. Activate the current environment in the terminal.
`julia --project=.`

5. Add a package.
`using Pkg`
`Pkg.add("VegaLite", preserve=PRESERVE_DIRECT)`

### 3. Working with Pluto
1. Add Pluto to the base julia installation
```Julia
]
add Pluto
```

2. Run Pluto
```Julia
using Pluto
Pluto.run()
```

#### Notebook Tips
- Save notebook in project folder ('name_of_env') not in the source folder ('name_of_env/src').
- Activate the environment in a new notebook
- Use markdown where needed.
- Use interactive envrionment where needed.

### 4. Working with Juno (VS Code)
1. Download the Julia for VS Code Extension: [Julia for VS Code](https://code.visualstudio.com/docs/languages/julia)

