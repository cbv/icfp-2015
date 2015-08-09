structure Phrases =
struct

  (* These are known to be correct *)
  val power =
    [ "Ei!" (* Example in contest doc. *)
    , "Ia! Ia!" (* Filled cells in problem 3. Confirmed in experiment. *)
    , "R'lyeh" (* filled cells in problem 5. Confirmed in experiment. *)
    , "Yuggoth" (* filled cells in problem 7.  Confirmed in experiment powere92. *)
    , "Cthulhu fhtagn!" (* "Cthulhu waits"; experiment powerc6e *)
    , "Necronomicon" (* for summoning the Old Ones; experiment power27e *)
    , "Tsathoggua" (* Master of formless spawn, tag manual515 *)
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
  (* bunch of junk that contains no power phrases; experiment power588/power27e *)
  , "aleister crowley alhazred enochian Corpus Hermeticum Azathoth Aiwaz nathan of gaza Klippoth"
  , "klaatu barada nikto" (* worth a shot; experiment power811 *)
  (* more random cthulhu junk; experiment power777 *)
  , "Al Azif Madame Blavatsky Salem Kadath UAAAH 'Umr at Tawil At U'mr ....... Tawil At'Umr"
  , "Azathoth" (* refuted by tag azathoth33b *)
  ]
end
