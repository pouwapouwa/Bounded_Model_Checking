(* $Id: BoundedModelChecking.mli 4109 2016-11-15 15:15:18Z sutre $ *)


(**
 * Bounded Model-Checking.
 *
 * This module solves the bounded model-checking problem for program automata
 * (see {! Automaton}).  The implementation looks for a feasible path from the
 * initial location to the final location.  It proceeds through a depth-first
 * exploration of the program automaton (viewed as a graph).
 *)


(**
 * Type of possible results of the [search] function.
 *)
type result =
  | Path of (Command.t * Automaton.Node.t) list
  (**
   * Feasible path from the initial location to the final location.
   * The initial location is omitted from the path.  Formally, the path is given
   * as a list {i (cmd{_ 1}, loc{_ 1}), ..., (cmd{_ k}, loc{_ k})} which stands
   * for the path {i init, cmd{_ 1}, loc{_ 1}, ..., cmd{_ k}, loc{_ k}}.
   *)
  | Empty of bool
  (**
   * No feasible path was found.  The boolean indicates the exhaustivity of the
   * result.  If the boolean is [true], then the search was exhaustive, meaning
   * that the program automaton under analysis contains no run from its initial
   * location to its final location. If the boolean is [false], then the search
   * was not exhaustive.
   *)

(**
 * [search automaton bound] solves the bounded model-checking problem for the
 * input program [automaton] and the input [bound].
 * {ul
 *   {li If it returns [Path p] then [p] is a feasible path of [automaton] of
 *       length at most [bound].}
 *   {li If it returns [Empty true] then [automaton] contains no feasible path.}
 *   {li If it returns [Empty false] then [automaton] contains no feasible path
 *       of length at most [bound].}
 * }
 * {L {b Requires:} [bound] is a nonnegative integer.}
 *)
val search : Automaton.t -> int -> result
