(*
  * This file defines the main program. An interface to interact with.
  * @author {andreu.barro, manoel.folgueira, uxia.ponte.villaverde}@udc.es
*)


(* Exception Definitions *)
exception MalformedDfaException;;


(* Modules *)
open Genlist;;
open State;;
open Symbol;;
open Transition;;
open Alphabet;;
open Dfa;;

open Str;;
open String;;


(*
 * [PRIVATE]
 * get_dfa_from_file : string -> string
 *
 * Gets a DFA's string form from a file located in 'path'
 *)
let get_dfa_from_file path =
  let ic = open_in path in
  try
    let line = input_line ic in
    flush stdout;
    close_in ic;
    line
  with e ->
    close_in_noerr ic;
    raise e
;;


(*
 * [PRIVATE]
 * parse_dfa : string -> ('a, 'b) Dfa.dfa
 *
 * Parses a DFA's well-formed string form into an actual DFA entity.
 *)
let parse_dfa dfa_string =
  let dfa_string_wols =
    String.sub dfa_string 0 ((String.length dfa_string) - 1)
  in
  match
    List.map (Str.split (Str.regexp " ")) (Str.split (Str.regexp "; ") dfa_string_wols)
  with
  | q::e::s::f::t ->
    let states = Genlist (List.map State.__new__ q)
    and alphabet = Alphabet (Genlist (List.map Symbol.__new__ e))
    and initial_state = match s with
      | [rstate] -> State.__new__ rstate
      | _ -> raise MalformedDfaException
    and final_states = Genlist (List.map State.__new__ f)
    and transitions =
      Genlist (
        List.map (
          function
            | [qi;qj;sym] -> Transition(State qi, State qj, Symbol sym)
            | _ -> raise MalformedDfaException
        ) t
      )
    in
      Dfa(states, alphabet, initial_state, final_states, transitions)
  | _ -> raise MalformedDfaException
;;


(*
 * main : unit -> unit
 *
 * This is the main routine of the application. It just receives an input
 * and returns an output via the standard io respectively.
 *
 * Check the "readme" file for further information.
 *)
let main () =
  let dfa_string =
    try
      get_dfa_from_file Sys.argv.(1)
    with _ ->
      Sys.argv.(1)
  in
  let dfa_in = parse_dfa dfa_string in
  let dfa_out = Dfa.get_connected_dfa dfa_in in
  print_endline (
    "\n-# INPUT DFA #-->\n" ^
    (Dfa.string_of_dfa dfa_in) ^ "\n\n" ^
    "-# OUTPUT DFA #-->\n" ^
    (Dfa.string_of_dfa dfa_out)
  )
;;


(* Let's run the application *)
main ();;
