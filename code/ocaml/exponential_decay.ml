(* Exponential decay: y = e^(-x), sampled on [0, 6].
   Prints "x y" pairs to stdout; pipe into gnuplot to see the curve:

     ocaml exponential_decay.ml | gnuplot -p -e "plot '-' with lines title 'exp(-x)'"

   or, for an ASCII plot right in the terminal:

     ocaml exponential_decay.ml | gnuplot -e "set terminal dumb; plot '-' with lines title 'exp(-x)'"
*)

let exponential_decay x = Float.exp (-. x)

let () =
  let n = 61 in
  for i = 0 to n - 1 do
    let x = 6. *. float_of_int i /. float_of_int (n - 1) in
    Printf.printf "%g %g\n" x (exponential_decay x)
  done
