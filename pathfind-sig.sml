signature PATHFIND = sig
  datatype target = Target of {px: int, py: int, a: int}
  val find : Board.state -> target -> Board.command list option
end
