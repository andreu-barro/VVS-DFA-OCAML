(*
  * This file defines the State element of a DFA.
  * @author {andreu.barro, manoel.folgueira, uxia.ponte.villaverde}@udc.es
*)


(* Type Definitions *)
type state = State of string;;


(* __new__ : string -> state *)
let __new__ e = State(e);;


(*
 * get_state : state -> string
 *
 * Returns the inner element of the state type
 *)
let get_state (State(e)) = e;;


(*
 * string_of_state : state -> string
 *
 * Returns a string representation of the state.
 *)
let string_of_state s = get_state s;;


(*
 * equals : state -> state -> bool
 *
 * Returns true if the given states are semantically equal.
 *)
let equals (State(x)) (State(y)) = (x = y);;
