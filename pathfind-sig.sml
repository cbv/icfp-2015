signature PATHFIND = sig
  val find : Board.state -> {px: int, py: int, a: int} -> Board.command list option
end
