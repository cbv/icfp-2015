signature POWERIZE =
sig

  (* Given state, target, power word, try to do power word and get to target *)
  val insert : Board.state -> Pathfind.target -> string -> Board.legalchar list option

end
