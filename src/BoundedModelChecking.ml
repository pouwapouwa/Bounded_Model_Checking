(* $Id: BoundedModelChecking.mli 4109 2016-11-15 15:15:18Z sutre $ *)


(**
 * Bounded Model-Checking.
 *
 * This module solves the bounded model-checking problem for program automata
 * (see {! Automaton}).  The implementation looks for a feasible path from the
 * initial location to the final location.  It proceeds through a depth-first
 * exploration of the program automaton (viewed as a graph).
 *)

open Z3

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

(**
 * Deep-First-Search function, parsing every subtrees of the last (first
 * in the list) node in 'previous_path'. Return a path if it is feasible
 * from this node to a final one. Otherwise, it will be a Empty (boolean) one,
 * saying if it's really not feasible (Empty true) or if we didn't parse through
 * every path, larger than the max_depth given.
 **)

(*
 * Z3.Solver.solver -> Z3.context -> Automaton.t ->
 * int -> int -> (Automaton.Label.t * Automaton.Node.t) list
 * -> result *)
let rec dfs solv ctx automaton max_depth depth previous_path =
  if depth > max_depth then
    begin
      Solver.pop solv 1;
      Empty false
    end
  else
    let actual_node =
      if (List.length previous_path == 0) then
        Automaton.initial automaton
      else
        match List.nth previous_path 0 with
        | (_, n) -> n
    in
    let res = (* Test if the node is a final one and if path is feasible *)
      if (Automaton.Node.equal (Automaton.final automaton) actual_node) then
        let status = Solver.check solv []
        in
        match status with
        | Solver.UNSATISFIABLE -> Empty true
        | Solver.SATISFIABLE   -> Path []
        | Solver.UNKNOWN       -> Empty true
      else
        Empty true
      in
      let final_result =
        List.fold_left
          (fun result (edge, node) ->
            match result with
            | Path  _ -> (* We previously found a path *)
               result
            | Empty e -> (* We previously found nothing feasible *)
               Solver.push solv;
               Solver.add solv (
                            Semantics.formula
                              ctx (Automaton.variables automaton) depth edge);
               let tmp = (* Recursive call to parse every succesors *)
                 dfs solv ctx automaton max_depth (depth + 1)
                     ((edge, node) :: previous_path)
               in
               match tmp with
               | Path  p  -> Path ((edge, node) :: p)
               | Empty e' -> Empty (e && e')
          )
          res
          (Automaton.succ automaton actual_node)
      in
      Solver.pop solv 1; (* Pop from the formula unwanted path *)
      final_result

(**
 * Main function od BoundelModelChecking module.
 * Take an automaton and a depth as parameter and apply a Deep-First-Search.
 * If a path of this depth if feasible to the final state, it returns it.
 **)

(*
 * Automaton.t -> int
 * -> result *)
let search automaton i =
  let context = mk_context []
  in
  let solver = Solver.mk_simple_solver context
  in
  Solver.push solver;
  dfs solver context automaton i 0 []
