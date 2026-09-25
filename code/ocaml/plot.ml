#use "topfind";;
#require "owl-plplot";;

module Plot = Owl_plplot.Plot

let exponential_decay x = Float.exp (1. /. x)

let () =
  let h = Plot.create "exponential_decay.png" in
  Plot.set_xlabel h "x";
  Plot.set_ylabel h "y";
  Plot.plot_fun ~h exponential_decay 1. 6.;
  Plot.output h