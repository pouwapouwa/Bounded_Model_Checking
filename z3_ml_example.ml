(* $Id $ *)

(*
 * Example usage of the Z3 OCaml API.  This file is loosely inspired from the
 * examples/ml/ml_example.ml file of the Z3 source distribution.
 *
 * The documentation of the API can be browsed at:
 *
 * http://z3prover.github.io/api/html/ml/Z3.html
 *
 * It is also strongly recommended to read sections 1, 2, 3 and 5 of the Z3
 * tutorial:
 *
 * http://rise4fun.com/z3/tutorial
 *)


open Z3

(* Display the Z3 version. *)
let _ =
  Format.printf "@[Running Z3 version %s@]@." Version.to_string

(*
 * Create a Z3 context.  We only need to create it once and then pass it to the
 * Z3 functions.
 *)
let ctx = mk_context []

(*
 * In the following, we build several formulas and check their satisfiability.
 *)

(* Build the formula f which is defined to be (x + y >= 2). *)
let f =
  (* Create two integer constants named "x" and "y". *)
  let x = Arithmetic.Integer.mk_const_s ctx "x"
  and y = Arithmetic.Integer.mk_const_s ctx "y"
  (* Create the integer numeral 2. *)
  and two = Arithmetic.Integer.mk_numeral_i ctx 2
  in
  (* Create the expression x + y. *)
  let x_plus_y = Arithmetic.mk_add ctx [x ; y]
  in
  (* Create the formula (x + y >= 2). *)
  Arithmetic.mk_ge ctx x_plus_y two

(* Display the formula f. *)
let _ =
  Format.printf "@[Formula f:@ %s@]@." (Expr.to_string f)

(* Check satisfiability of the formula f. *)
let _ =
  let solver = (Solver.mk_simple_solver ctx) in
  (* Assert the formula f into the solver. *)
  Solver.add solver [f] ;
  (* Check satisfiability of the solver's assertions. *)
  let status = Solver.check solver [] in
  Format.printf "@[Solver says about f: %s@]@." (Solver.string_of_status status)

(* Build the formula g which is defined to be (x < 0) ∧ (y = 2). *)
let g =
  (* Create two integer constants named "x" and "y". *)
  let x = Arithmetic.Integer.mk_const_s ctx "x"
  and y = Arithmetic.Integer.mk_const_s ctx "y"
  (* Create the integer numerals 0 and 2. *)
  and zero = Arithmetic.Integer.mk_numeral_i ctx 0
  and two = Arithmetic.Integer.mk_numeral_i ctx 2
  in
  (* Create the formulas x < 0 and y = 2. *)
  let x_negative = Arithmetic.mk_lt ctx x zero
  and y_equals_two = Boolean.mk_eq ctx y two
  in
  (* Return the conjunction of these two formulas. *)
  Boolean.mk_and ctx [x_negative ; y_equals_two]

(* Display the formula g. *)
let _ =
  Format.printf "@[Formula g:@ %s@]@." (Expr.to_string g)

(* Build the formula h which is defined to be x * (y - 3) <= 1. *)
let h =
  (* Create two integer constants named "x" and "y". *)
  let x = Arithmetic.Integer.mk_const_s ctx "x"
  and y = Arithmetic.Integer.mk_const_s ctx "y"
  (* Create the integer numerals 1 and three. *)
  and one = Arithmetic.Integer.mk_numeral_i ctx 1
  and three = Arithmetic.Integer.mk_numeral_i ctx 3
  in
  (* Create the expression y - 3. *)
  let y_minus_three = Arithmetic.mk_sub ctx [y ; three]
  in
  (* Create the expression x * (y - 3). *)
  let x_mult_y_minus_three = Arithmetic.mk_mul ctx [x ; y_minus_three]
  in
  (* Create the formula x * (y - 3) <= 1. *)
  Arithmetic.mk_le ctx x_mult_y_minus_three one

(* Display the formula h. *)
let _ =
  Format.printf "@[Formula h:@ %s@]@." (Expr.to_string h)

(*
 * We now check satisfiability of the formulas (f ∧ g) and (f ∧ h). Note the
 * use of backtracking with Solver.push and Solver.pop.
 *)

let _ =
  let solver = (Solver.mk_simple_solver ctx) in
  (* Assert the formula f into the solver. *)
  Solver.add solver [f] ;
  (* Create a backtracking point. *)
  Solver.push solver ;
  (* Assert the formula g into the solver. *)
  Solver.add solver [g] ;
  begin
    (* Check satisfiability of the solver's assertions. *)
    let status = Solver.check solver [] in
    Format.printf "@[Solver says about f ∧ g: %s@]@." (Solver.string_of_status status)
  end ;
  (* Backtrack. *)
  Solver.pop solver 1 ;
  (* Assert the formula g into the solver. *)
  Solver.add solver [h] ;
  begin
    (* Check satisfiability of the solver's assertions. *)
    let status = Solver.check solver [] in
    Format.printf "@[Solver says about f ∧ h: %s@]@." (Solver.string_of_status status)
  end

(* Exit. *)
let _ =
  Format.printf "@[Exiting.@]@." ;
  exit 0
