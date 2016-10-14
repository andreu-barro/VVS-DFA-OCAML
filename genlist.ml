(*
  * This file defines a particular generic list.
  * @author {andreu.barro, manoel.folgueira, uxia.ponte.villaverde}@udc.es
*)


(* Exception Definitions *)
exception GenlistException;;


(* Type Definitions *)
type 'a genlist = Genlist of 'a list;;


(* __new__ : unit -> 'a genlist *)
let __new__ () = Genlist([]);;

(*
 * get_existing_element : 'a genlist -> 'a -> ('a -> 'a -> bool) -> 'a
 *
 * Returns the first 'a element 'x' from the given 'a Genlist.genlist 'g' that
 * satisfies the given compartor 'cmp' alongside the given 'a element 'e'.
 *)
let rec get_existing_element (g: 'a genlist) (e: 'a) cmp = match g with
  | Genlist(h::t) ->
    if cmp h e
    then h
    else get_existing_element (Genlist(t)) e cmp
  | _ -> raise GenlistException
;;


(*
 * add : 'a genlist -> 'a -> 'a genlist
 *
 * Adds an element to the end of the genlist.
 *)
let add (Genlist(l)) e = Genlist(l@[e]);;


(*
 * get : 'a genlist -> int -> 'a
 *
 * Gets the nth element of the genlist.
 *)
let get g n =
  let rec get_rec g n i = match g with
    | Genlist(h::_) when i=n -> h
    | Genlist(_::t) ->
      get_rec (Genlist(t)) n (i+1)
    | _ -> raise GenlistException
  in get_rec g n 0
;;


(*
 * get_list : 'a genlist -> 'a list
 *
 * Returns the inner list structure of the given genlist.
 *)
let get_list (Genlist(l)) = l;;


(*
 * string_of_genlist : ('a -> string) -> 'a genlist -> string
 *
 * A string representation of the genlist.
 *)
let string_of_genlist str_converter g =
  let rec string_of_genlist_rec g str = match g with
    | Genlist([]) -> ""
    | Genlist([h]) -> str ^ (str_converter h)
    | Genlist(h::t) ->
      string_of_genlist_rec (Genlist(t)) (str ^ (str_converter h) ^ ", ")
  in string_of_genlist_rec g ""
;;
