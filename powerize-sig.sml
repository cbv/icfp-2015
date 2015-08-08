signature POWERIZE =
sig

  (* boolean given to continuation is whether all moves succeeded without
     GAMEOVER, ERROR, or locking *)
  val move_unwind_many : Board.state * Board.legalchar list * (bool -> 'a) -> 'a

  (* Given state, target, power word, try to do power word and get to target *)

  val insert : Board.state -> Pathfind.target -> string -> Board.legalchar list option

end
