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
 * all variables.  Typically, X is the of variables of the program automaton (see
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
val formula : Z3.context -> Variable.t list -> int -> Command.t -> Z3.Expr.expr list
