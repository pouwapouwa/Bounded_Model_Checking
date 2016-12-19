(* $Id: Semantics.mli 4118 2016-11-15 21:39:22Z sutre $ *)


(**
 * Semantics of program commands.
 *
 * This module translates program commands (see {! Command}) into [Z3] formulas.
 * The latter are values of type [Z3.Expr.expr].
 * The translation is given by the formula {i cmd{^ (i)}} where {i cmd} is a
 * command and {i i} is an index.  This formula is presented in the project
 * assignment.
 *
 * Let us recall that the set of free variables of the formula {i cmd{^ (i)}}
 * is {i \{x{^(i)}, x{^(i+1)}} | x ∈ X\}, where X is the underlying set of
 * all variables.  Typically, X is the variables of the program automaton (see
 * {! Automaton}) under analysis.  The implementation of this module encodes
 * superscripts with a dollar sign.  For instance, assuming that [foo] is a
 * variable, [foo]{^ (3)} is encoded by [foo$3].  This cannot cause confusion
 * since variables of program automata contain no dollar sign (the lexer
 * {! AutLexer} enforces this property).
 *
 * @see <http://z3prover.github.io/api/html/ml/Z3.Expr.html> Z3.Expr
 *)


(**
 * [formula ctx vars i cmd] returns the translation [cmd]{^ ([i])} of a command
 * [cmd] for an index [i].  The underlying set of variables is given by the list
 * [vars].  The translation is returned as a list [f1; ...; fN] of formulas,
 * which stands for the conjunction f1 ∧ ... ∧ fN.
 * {L {b Requires:} [i] is a nonnegative integer, and every variable occuring in
 * [cmd] also occurs in [vars].}
 *)

(*
 * -----------------------------------------------------------------------------
 * -----------------------------------------------------------------------------
 *)

open Command
open Z3
open Z3.Arithmetic
open Z3.Boolean

(**
 * Function matching expressions
 * Return the list of clauses in order to respect the expression expr
 * All this occuring in the context ctx, at rank i
 **)

(*
 * val matching_expression:
 * Z3.context -> Command.Predicate.t -> int
 * -> Z3.Expr.expr list *)
let rec matching_expression ctx expr i =
  match expr with
  | Expression.Cst c           ->
     (Integer.mk_numeral_i ctx c, [])
  | Expression.Var var         ->
     (Integer.mk_const_s ctx (var ^ "$" ^ string_of_int i), [])
  | Expression.Op (e1, op, e2) ->
     let e, l =
       matching_expression ctx e1 i
     and e', l'=
       matching_expression ctx e2 i
     in
     match op with
     | Expression.Add  -> (mk_add ctx [e; e'], l @ l')
     | Expression.Sub  -> (mk_sub ctx [e; e'], l @ l')
     | Expression.Mul  -> (mk_mul ctx [e; e'], l @ l')
     | Expression.Div  ->
        let list =
          [mk_not ctx (mk_eq ctx e' (Integer.mk_numeral_i ctx 0))]
        in
        (mk_div ctx e e', list @ l @ l')

(**
 * Function matching predicates
 * Return the list of clauses in order to respect the predicate pred
 * All this occuring in the context ctx, at rank i
 **)

(*
 * val matching_predicate:
 * Z3.context -> Command.Predicate.t -> int
 * -> Z3.Expr.expr list *)
let matching_predicate ctx pred i =
  match pred with
  | (e1, op, e2) ->
     let e, l =
       matching_expression ctx e1 i
     and e', l' =
       matching_expression ctx e2 i
     in
     match op with
     | Predicate.Eq    -> (mk_eq ctx e e', l @ l')
     | Predicate.Lst   -> (mk_lt ctx e e', l @ l')
     | Predicate.Gst   -> (mk_gt ctx e e', l @ l')
     | Predicate.Leq   -> (mk_le ctx e e', l @ l')
     | Predicate.Geq   -> (mk_ge ctx e e', l @ l')

(**
 * Function linked to Assignation
 * Return the list of clauses to respect in order
 * to match the assignation leaded by expr, on the variable var
 * All this occuring in the context ctx, at rank i
 **)

(*
 * val assign:
 * Z3.context -> string -> Command.Expression.t -> int
 * -> Z3.Expr.expr list *)
let assign ctx var expr i =
  let x =
    Integer.mk_const_s ctx (var ^ "$" ^ (string_of_int (i+1)))
  and y, l =
    matching_expression ctx expr i
  in
  (mk_eq ctx x y, l)

(**
 * Function linked to Guard
 * Return the list of unchanged assignement for all variables in vars
 * All this occuring in the context ctx, at rank i
 * (index used to browse variable list)
 **)

(* val keepargs:
 * Z3.context -> Variable.t list -> string -> Command.Expression.t -> int -> int
 * -> Z3.Expr.expr list *)
let rec keepargs ctx vars i index =
  if index == List.length(vars) then
    []
  else
    let x =
      Integer.mk_const_s ctx ((List.nth vars index) ^ "$"
                                            ^ (string_of_int i))
    and y =
      Integer.mk_const_s ctx ((List.nth vars index) ^ "$"
                                            ^ (string_of_int (i+1)))
    in
    List.append [mk_eq ctx x y] (keepargs ctx vars i (index+1))

(**
 * Function linked to Assignation
 * Return the list of unchanged assignement for all variables in vars
 * Excepted for the var one, which is modified by the expression expr
 * All this occuring in the context ctx, at rank i
 * (index used to browse variable list)
 **)

(* val keepargs_except:
 * Z3.context -> Variable.t list -> string -> Command.Expression.t -> int -> int
 * -> Z3.Expr.expr list *)
let rec keepargs_except ctx vars var exp i index  =
  begin
    if index == List.length(vars) then
      []
    else
      let vartest = List.nth vars index
      in
      if (compare vartest var) == 0 then
        let a, l =
          assign ctx var exp i
        in
        List.append (List.append [a] l)
                    (keepargs_except ctx vars var exp i (index+1))
      else
        let tmp = Integer.mk_const_s ctx vartest
          in
          let x = Integer.mk_const_s ctx
                                     ((Expr.to_string tmp)
                                      ^ "$" ^ (string_of_int i))
          and y = Integer.mk_const_s ctx
                                     ((Expr.to_string tmp)
                                      ^ "$" ^ (string_of_int (i+1)))
          in
          List.append [mk_eq ctx x y]
                      (keepargs_except ctx vars var exp i (index+1))
  end

(**
 * Main function of the module
 **)

(* val formula :
 * Z3.context -> Variable.t list -> int -> Command.t
 * -> Z3.Expr.expr list *)
let formula ctx vars i cmd =
  match cmd with
  | Skip -> keepargs ctx vars i 0
  | Assign (var, expr) -> keepargs_except ctx vars var expr i 0
  | Guard pred ->
     let t, l =
       matching_predicate ctx pred i
     in
     List.append (List.append [t] l) (keepargs ctx vars i 0)
