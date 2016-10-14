(*
  * This file defines the Transition element of a DFA.
  * @author {andreu.barro, manoel.folgueira, uxia.ponte.villaverde}@udc.es
*)


(* Own Modules *)
open State;;
open Symbol;;


(* Type Definitions *)
type transition =
  Transition of (state * state * symbol);;


(* val __new__ : State.state * State.state * Symbol.symbol -> transition *)
let __new__ (State(_) as qi) (State(_) as qj) (Symbol(_) as sym) =
  Transition(qi, qj, sym);;


(*
 * get_start_state : transition -> State.state
 *
 * Returns the start state (qi) from the given transition.
 *)
let get_start_state (Transition(qi, _, _)) = qi;;


(*
 * get_end_state : transition -> State.state
 *
 * Returns the end state (qj) from the given transition.
 *)
let get_end_state (Transition(_, qj, _)) = qj;;


(*
 * get_symbol : transition -> Symbol.symbol
 *
 * Returns the symbol (sym) from the given transition.
 *)
let get_symbol (Transition(_, _, sym)) = sym;;


(*
 * string_of_transition : transition -> string
 *
 * Returns a string representation of the state.
 *)
let string_of_transition
  (Transition(qi, qj, sym)) =
    "Transition = {"
      ^ State.string_of_state qi ^ ", "
      ^ State.string_of_state qj ^ ", "
      ^ Symbol.string_of_symbol sym ^
    "}"
;;


(*
 * equals : transition -> transition -> bool
 *
 * Returns true if the given transitions are semantically equal.
 *)
let equals (Transition(qi, _, s)) (Transition(ri, _, t)) =
  ((State.equals qi ri) && (Symbol.equals s t));;
