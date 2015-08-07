structure Phrases =
struct

  (* These are known to be correct *)
  val power =
    [ "Ei!" (* Example in contest doc. *)
    , "Ia! Ia!" (* Filled cells in problem 3. Confirmed in experiment. *)
    , "R'lyeh" (* filled cells in problem 5. Confirmed in experiment. *)
    ]

  (* Ideas? *)
  val hypotheses = [
     (* Filled cells in problem 7. In experiment 'scarpy4' we learned that
        "ayuggoth" contains a power phrase. *)
    "Yuggoth"
  ]

end
