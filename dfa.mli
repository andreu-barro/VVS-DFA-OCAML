type dfa =
    Dfa of State.state Genlist.genlist * Alphabet.alphabet * State.state *
      State.state Genlist.genlist * Transition.transition Genlist.genlist
val __new__ :
  State.state Genlist.genlist ->
  Alphabet.alphabet ->
  State.state ->
  State.state Genlist.genlist -> Transition.transition Genlist.genlist -> dfa
val get_connected_dfa : dfa -> dfa
val string_of_dfa : dfa -> string
