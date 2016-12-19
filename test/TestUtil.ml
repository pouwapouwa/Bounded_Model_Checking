(* $Id: TestUtil.ml 4097 2016-11-14 17:10:54Z sutre $ *)


(*
 * Utility functions for unit tests.
 *)


(* Helper function to create test messages. *)
let message cmd k =
  Format.asprintf "@[<h>[%a]^%d@]" Command.print cmd k

(* Shortcuts to write expressions. *)
let cst c = Command.Expression.Cst c
let var v = Command.Expression.Var v
let ( + ) e e' = Command.Expression.Op (e, Command.Expression.Add, e')
let ( - ) e e' = Command.Expression.Op (e, Command.Expression.Sub, e')
let ( * ) e e' = Command.Expression.Op (e, Command.Expression.Mul, e')
let ( / ) e e' = Command.Expression.Op (e, Command.Expression.Div, e')

(* Shortcuts to write guards. *)
let ( == ) e e' = Command.Guard (e, Command.Predicate.Eq, e')
let ( < ) e e' = Command.Guard (e, Command.Predicate.Lst, e')
let ( > ) e e' = Command.Guard (e, Command.Predicate.Gst, e')
let ( <= ) e e' = Command.Guard (e, Command.Predicate.Leq, e')
let ( >= ) e e' = Command.Guard (e, Command.Predicate.Geq, e')

(* Shortcut to write assignments. *)
let ( := ) v e =
  match v with
  | Command.Expression.Var v -> Command.Assign (v, e)
  | Command.Expression.Cst _
  | Command.Expression.Op _ -> invalid_arg "TestUtil.(:=)"

(* Shortcut to write skips. *)
let skip = Command.Skip
