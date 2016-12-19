(* $Id: bmc.ml 4104 2016-11-15 13:12:34Z sutre $ *)


(*
 * Main (start of the spa program).
 *)


(* Exception for errors that are local to this module (should not happen). *)
exception Internal_error

(* Module to parse the command-line and store user-specified options. *)
module CommandLine =
struct
  (* Bound on the length of paths. *)
  let bound_opt = ref 10

  (* Specification of command-line options. *)
  let arg_spec_list = [
    ("-bound",
     Arg.Int
       (fun n ->
        if n >= 0 then bound_opt := n
        else
          raise (Arg.Bad ("Incorrect negative value `" ^ (string_of_int n) ^
                            "' for option -bound"))),
     "<int> bound on the length of paths" ^
       " (default: " ^ (string_of_int !bound_opt) ^ ")")
  ]

  let usage_msg = "Usage: " ^ (Sys.argv.(0)) ^ " [option ...] [source-file]\n"

  (* Parses the command line and returns the input file's name, if any. *)
  let parse () =
    let filename = ref None
    in
    Arg.parse
      (Arg.align arg_spec_list)
      (fun a ->
       match !filename with
       | None ->
          filename := Some a
       | Some _ ->
          raise (Arg.Bad ("unexpected argument `" ^ a ^
                            "' (multiple input files are not allowed)")))
      usage_msg ;
    !filename
end

(* Parse the command line. *)
let filename = CommandLine.parse ()

(* Parse the input file. *)
let automaton =
  let (ic, fn) =
    match filename with
    | None -> (stdin, "<stdin>")
    | Some f -> (open_in f, f)
  in
  let a =
    try
      Automaton.read ic
    with Automaton.Read_error msg ->
      Format.eprintf "@[%s:@ %s@]@." fn msg ;
      close_in ic ;
      exit 1
  in
  close_in ic ;
  a

(* Print the automaton. *)
let _ =
  Format.printf "@[%a@]@." Automaton.print automaton

(* Perform bounded model-checking and display the result. *)
let _ =
  match
    BoundedModelChecking.search automaton (!CommandLine.bound_opt)
  with
  | BoundedModelChecking.Path path ->
     Format.printf
       "@.@[<v 3>Feasible path:@,@[<v 3>%a"
       Automaton.Node.print
       (Automaton.initial automaton) ;
     List.iter
       (fun (cmd, loc) ->
        Format.printf
          "@,@[<h>»@ @[%a@]@ »@]@]@,@[<v 3>%a"
          Command.print cmd
          Automaton.Node.print loc)
       path ;
     Format.printf "@]@]@."
  | BoundedModelChecking.Empty false ->
     Format.printf
       "@.@[No feasible path of length <= %d@]@." !CommandLine.bound_opt
  | BoundedModelChecking.Empty true ->
     Format.printf
       "@.@[No feasible path@]@."

(* Exit. *)
let _ = exit 0
