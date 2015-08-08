signature PATHFIND = sig
  datatype target = Target of {px: int, py: int, a: int}
  val find : Board.state -> target -> Board.legalchar list option

  (* the (int -> string) function is an indexed sequence of phrases of power
     we'll try to achieve *)
  val find_with_power : Board.state -> target -> (int -> string) -> Board.legalchar list option

end
