(*
  * This file defines the Alphabet element of a DFA.
  * @author {andreu.barro, manoel.folgueira, uxia.ponte.villaverde}@udc.es
*)


(* Own Modules *)
open Symbol;;
open Genlist;;


(* Type Definitions *)
type alphabet = Alphabet of symbol genlist;;


(* __new__ : unit -> alphabet *)
let __new__ () = Alphabet(Genlist([]));;


(*
 * add_new_symbol : alphabet -> Symbol.symbol -> alphabet
 *
 * Adds a new symbol to the current alphabet.
 *)
let add_new_symbol (Alphabet(g)) (Symbol(_) as sym) =
  Alphabet(Genlist.add g sym);;


(*
 * get_existing_element : alphabet -> Symbol.symbol -> Symbol.symbol
 *
 * Uses and is equivalent to Genlist.get_existing_element
 *)
let get_existing_element (Alphabet(g)) (Symbol(_) as sym) =
  Genlist.get_existing_element g sym Symbol.equals;;


(*
 * get_genlist : alphabet -> Symbol.symbol Genlist.genlist
 *
 * Returns the inner Symbol.symbol Genlist.genlist of the current alphabet.
 *)
let get_genlist (Alphabet(g)) = g;;


(*
 * string_of_alphabet : alphabet -> string
 *
 * Uses and is equivalent to Genlist.string_of_genlist
 *)
let string_of_alphabet (Alphabet(g)) =
  string_of_genlist Symbol.string_of_symbol g;;
