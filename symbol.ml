(*
  * This file defines the Symbol element of the Alphabet.
  * @author {andreu.barro, manoel.folgueira, uxia.ponte.villaverde}@udc.es
*)


(* Type Definitions *)
type symbol = Symbol of string;;


(* val __new__ : string -> symbol *)
let __new__ e = Symbol(e);;


(*
 * get_symbol : symbol -> string
 *
 * The inner element of the symbol.
 *)
let get_symbol (Symbol(e)) = e;;


(*
 * string_of_symbol : symbol -> string
 *
 * Returns a string representation of the symbol.
 *)
let string_of_symbol s = get_symbol s;;


(*
 * equals : symbol -> symbol -> bool
 *
 * Returns true if the given symbols are semantically equal.
 *)
let equals (Symbol(x)) (Symbol(y)) = (x = y);;
