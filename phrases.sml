structure Phrases =
struct

  (* These are known to be correct *)
  val power =
    [ "Ei!" (* Example in contest doc. *)
    , "Ia! Ia!" (* Filled cells in problem 3. Confirmed in experiment. *)
    , "R'lyeh" (* filled cells in problem 5. Confirmed in experiment. *)
    , "Yuggoth" (* filled cells in problem 7.  Confirmed in experiment powere92. *)
    , "bapl"
    ]

  (* Ideas? *)
  val hypotheses = [
    (* Other Cthulhu mythos stuff?  Yog Suthuth, Shub Niggurath...? *)
    "Shub Niggurath!" (* inconclusive -- experiment power4bb may have a power word,
                         but it could have been the submission tagged 'david2'
                         instead -- not sure there's a way to tell via the
                         leaderboard.. *)
  (* Maybe check the new problem, which drops letters.  -tom7 *)
  ]

  (* These are known to be incorrect -- phrases of weakness *)
  val weakness = [
    "Cthulhu Cthulhu Cthulhu" (* from contest clarification -- refuted by experiment powerd1f *)
  , "Yog Sothoth!" (* cthulhu mythos -- refuted by experiment power652 *)
  , "Shub" (* shub alone is not enough -- refuted by experiment powerca8 *)
  ]
end
