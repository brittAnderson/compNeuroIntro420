{
  description = "Python and R environments for running student scripts";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      python = pkgs.python3.withPackages (ps: with ps; [
        numpy matplotlib tkinter jupyter ipykernel
      ]);
      renv = pkgs.rWrapper.override {
        packages = with pkgs.rPackages; [
          ggplot2
          knitr
          rmarkdown
          fcaR
          ggraph
        ];
      };
    in {
      devShells.${system}.default = pkgs.mkShell {
        packages = [
          python
          renv
          pkgs.tk
          pkgs.quarto
          pkgs.pandoc
        ];
        shellHook = ''
            export QUARTO_R=${renv}/bin/R
            export QUARTO_PYTHON=${python}/bin/python
              '';       
      };
    };
}
