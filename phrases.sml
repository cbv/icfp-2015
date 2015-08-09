structure Phrases =
struct

  (* These are known to be correct *)
  val power =
    [ "Ei!" (* Example in contest doc. *)
    , "Ia! Ia!" (* Filled cells in problem 3. Confirmed in experiment. *)
    , "R'lyeh" (* filled cells in problem 5. Confirmed in experiment. *)
    , "Yuggoth" (* filled cells in problem 7.  Confirmed in experiment powere92. *)
    , "Cthulhu fhtagn!" (* "Cthulhu waits", experiment powerc6e *)
    ]

  (* Ideas? *)
  val hypotheses = [
    (* Other Cthulhu mythos stuff?  Yog Suthuth, Shub Niggurath...? *)
  ]

  (* These are known to be incorrect -- phrases of weakness *)
  val weakness = [
    "Cthulhu Cthulhu Cthulhu" (* from contest clarification -- refuted by experiment powerd1f *)
  , "Yog Sothoth!" (* cthulhu mythos -- refuted by experiment power652 *)
  , "Shub" (* shub alone is not enough -- refuted by experiment powerca8 *)
  , "bap" (* discovered via problem 1 score, but refuted by experiment power638 (leaderboard "isomorphic phrasing" bug). *)
  , "davar" (* refuted by experiment power4df *)
  , "icfp2015" (* dropped letters in problem 24.. /probably/ not?  fp is E W, and so is 20.. very hard not to stutter *)
  , "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn!" (* experiment power17c *)
  , "fnord" (* experiment power26f *)
  , "Shub Niggurath!" (* ultimately refuted by experiment power592 *)
  , "Nithon!" (* a moon of Yuggoth; refuted by experiment power436 *)
  , "Thog!" (* a twin moon of Yuggoth; refuted by experiment powerf29 *)
  (* three refuted by power 1c9 *)
  , "Thok!" (* a twin moon of Yuggoth; refuted by experiment power1c9 *)
  , "Tok'l" (* a kind of metal mined on Yuggoth; refuted by experiment power1c9 *)
  , "Shoggoth!" (* monster from Yuggoth; refuted by experiment power1c9 *)
  , "Innsmouth" (* Lovecraftian port town; refuted by experiment power3a4 *)
  , "Miskatonic" (* Lovecraftian university; refuted by experiment power8e5 *)
    (* NB: Miskatonic University is hard to make work -- "rs" is a CW/CCW *)
  ]
end
