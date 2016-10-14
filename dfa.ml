(*
  * This file defines the DFA entity.
  * @author {andreu.barro, manoel.folgueira, uxia.ponte.villaverde}@udc.es
*)


(* Own Modules *)
open Genlist;;
open State;;
open Symbol;;
open Alphabet;;
open Transition;;


(* Type Definitions *)
type dfa =
  Dfa of
    state genlist *
    alphabet *
    state *
    state genlist *
    transition genlist
;;


(* __new__ :
  State.state Genlist.genlist ->
  Alphabet.alphabet ->
  State.state ->
  State.state Genlist.genlist ->
  Transition.transition Genlist.genlist ->
  dfa *)
let __new__ q e s f t = Dfa(q, e, s, f, t);;

(*
 * [PRIVATE]
 * get_all_connected_states : dfa -> State.state Genlist.genlist
 *
 * Filters all the connected states for any arbitrary DFA.
 *)
let get_all_connected_states ((Dfa(q, e, s, f, t)) as dfa) =
  let rec get_all_connected_states_rec
      (Dfa(q, Alphabet(Genlist(e)), s, f, t) as dfa) queue v =
    match queue with
      | [] -> Genlist(v)
      | hstate::tstate ->
        let q, states = (
          let rec get_h_transitions_rec e queue v = match e with
            | [] -> queue, v
            | hsym::tsym ->
              (* qj in Transition.equals is not compared *)
              let new_transition = Transition(hstate, hstate, hsym) in
              try
                  let real_transition =
                    Genlist.get_existing_element t new_transition Transition.equals
                  in
                  try
                    let _ = Genlist.get_existing_element
                      (Genlist(v))
                      (Transition.get_end_state real_transition)
                      State.equals in ();
                    get_h_transitions_rec tsym queue v
                  with
                  | _ ->
                  get_h_transitions_rec
                    tsym
                    (queue@[Transition.get_end_state real_transition])
                    (v@[Transition.get_end_state real_transition])
              with
              | _ -> get_h_transitions_rec tsym queue v
          in get_h_transitions_rec e tstate v
        )
        in get_all_connected_states_rec dfa q states
  in get_all_connected_states_rec dfa [s] [s]
;;


(*
 * get_connected_dfa : dfa -> dfa
 *
 * Transforms any given arbitrary DFA into its connected one.
 *)
let get_connected_dfa ((Dfa(q, e, s, f, t)) as dfa) =
  let new_q = get_all_connected_states dfa in
  (* e and s reamin the same *)
  let new_f =
    let rec eliminate_f (Genlist(f)) q o = match f with
      | [] -> o
      | fh::ft ->
        try
            let _ = Genlist.get_existing_element q fh State.equals in ();
            eliminate_f (Genlist(ft)) q (o@[fh])
        with
        | _ -> eliminate_f (Genlist(ft)) q o
    in eliminate_f f new_q []
  and new_t =
    let rec eliminate_t (Genlist(t)) q o = match t with
      | [] -> o
      | th::tt ->
        try
          let startState = Transition.get_start_state th
          in
            let _ = Genlist.get_existing_element q startState State.equals in ();
            eliminate_t (Genlist(tt)) q (o@[th])
        with
        | _ -> eliminate_t (Genlist(tt)) q o
    in eliminate_t t new_q []
  in
    Dfa(new_q, e, s, Genlist(new_f), Genlist(new_t))
;;


(*
 * [PRIVATE]
 * get_transitions_table : dfa -> string
 *
 * The transitions structure as a string table
 *)
let get_transitions_table (Dfa(Genlist(q), Alphabet(Genlist(syml)), s, f, tra)) =
  let output = (
      "   | " ^
      (
        let rec symbols = function
          | [] -> ""
          | (Symbol s)::t -> s ^ "  | " ^ (symbols t)
        in symbols syml
      ) ^ "\n" ^
      (
        let rec states = function
          | [] -> ""
          | (State state)::tstate ->
            "\t" ^ state ^ " | " ^
            (
              let rec entries = function
                | [] -> ""
                | (Symbol sym)::tsym ->
                let tr = Transition(State state, State state, Symbol sym) in
                try
                  let Transition(_, end_state, _) =
                    Genlist.get_existing_element tra tr Transition.equals in
                  (State.string_of_state end_state) ^ " | " ^ (entries tsym)
                with
                | _ -> "  " ^ " | " ^ (entries tsym)
              in entries syml
            ) ^ "\n" ^ (states tstate)
        in states q
      )
  ) in output
;;


(*
 * string_of_dfa : dfa -> string
 *
 * DFA's string representation
 *)
let string_of_dfa (Dfa(q, Alphabet(ge), s, f, t) as dfa) =
  "DFA = {\n" ^
    "\tstates = {" ^ Genlist.string_of_genlist State.string_of_state q ^ "},\n" ^
    "\talphabet = {" ^ Genlist.string_of_genlist Symbol.string_of_symbol ge ^ "},\n" ^
    "\tinitial_state = {" ^ State.string_of_state s ^ "},\n" ^
    "\tfinal_states = {" ^ Genlist.string_of_genlist State.string_of_state f ^ "},\n" ^
    "\ttransitions =\n\t" ^ get_transitions_table dfa ^ "\n" ^
  "}"
;;
