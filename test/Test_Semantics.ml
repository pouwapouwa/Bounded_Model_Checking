(* $Id: Test_Semantics.ml 4105 2016-11-15 13:17:39Z sutre modified 2016-11-30 19:12 $ *)


(*
 * Unit tests for the Semantics module.
 *)


open TestCore
open TestUtil
open Semantics

let x = var "x"
and y = var "y"
and z = var "z"

let variables = ["x" ; "y" ; "z"]

let string_of_z3_expr_list l =
  "{" ^ (String.concat ", " (List.map Z3.Expr.to_string l)) ^ "}"

let context = Z3.mk_context []

(* Generic test function. *)
let aux cmd k expected =
  assert_string
    ~msg:(message cmd k)
    expected
    (string_of_z3_expr_list (formula context variables k cmd))

let test_assign () =
  aux (x := cst 3) 0 "{(= x$1 3), (= y$0 y$1), (= z$0 z$1)}" ;
  aux (z := x - y) 2 "{(= x$2 x$3), (= y$2 y$3), (= z$3 (- x$2 y$2))}" ;
  aux (z := x / y) 1 "{(= x$1 x$2), (= y$1 y$2), (= z$2 (div x$1 y$1)), (not (= y$1 0))}"

let test_guard () =
  aux (x <= y + z) 0 "{(<= x$0 (+ y$0 z$0)), (= x$0 x$1), (= y$0 y$1), (= z$0 z$1)}" ;
  aux (x + cst 5 > y / z) 0 "{(> (+ x$0 5) (div y$0 z$0)), (not (= z$0 0)), (= x$0 x$1), (= y$0 y$1), (= z$0 z$1)}"

let test_skip () =
  aux (skip) 0 "{(= x$0 x$1), (= y$0 y$1), (= z$0 z$1)}"

let supp_test_assign () =
  aux (x := x - cst 1) 1 "{(= x$2 (- x$1 1)), (= y$1 y$2), (= z$1 z$2)}";
  aux (z := (x + cst 3) / y) 1 "{(= x$1 x$2), (= y$1 y$2), (= z$2 (div (+ x$1 3) y$1)), (not (= y$1 0))}";
  aux (z := (x / (y / cst 8))) 1 "{(= x$1 x$2), (= y$1 y$2), (= z$2 (div x$1 (div y$1 8))), (not (= (div y$1 8) 0)), (not (= 8 0))}";
  aux (z := (x + cst 3) / (y / cst 8)) 1 "{(= x$1 x$2), (= y$1 y$2), (= z$2 (div (+ x$1 3) (div y$1 8))), (not (= (div y$1 8) 0)), (not (= 8 0))}";
  aux (z := (x + cst 3) / (y / (z - cst 4))) 1 "{(= x$1 x$2), (= y$1 y$2), (= z$2 (div (+ x$1 3) (div y$1 (- z$1 4)))), (not (= (div y$1 (- z$1 4)) 0)), (not (= (- z$1 4) 0))}"

let supp_test_guard () =
  aux (((x + cst 5) / z) > y / z) 0 "{(> (div (+ x$0 5) z$0) (div y$0 z$0)), (not (= z$0 0)), (not (= z$0 0)), (= x$0 x$1), (= y$0 y$1), (= z$0 z$1)}";
  aux (((x - cst 5) / (z + cst 2)) < y / (z + y)) 0 "{(< (div (- x$0 5) (+ z$0 2)) (div y$0 (+ z$0 y$0))), (not (= (+ z$0 2) 0)), (not (= (+ z$0 y$0) 0)), (= x$0 x$1), (= y$0 y$1), (= z$0 z$1)}"

(* Collection of all tests. *)
let alltests =
  [
    "semantics.assign", test_assign ;
    "semantics.assign supp", supp_test_assign;
    "semantics.guard", test_guard ;
    "semantics.guard supp", supp_test_guard ;
    "semantics.skip", test_skip ;
  ]

(* This test suite. *)
let suite = ("Semantics over variables {" ^ (String.concat ", " variables) ^ "}", alltests)
