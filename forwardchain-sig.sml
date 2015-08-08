signature FORWARD_CHAIN = sig

  exception ForwardChain of string

  datatype LockedState = NEW_PIECE of Board.state | ALL_DONE

  datatype PieceLocation = PL of {px: int, py: int, a: int,
                                  (* If the last move was a lock, a cloned copy of the resulting state. *)
                                  locked: LockedState option,
                                  (* Points accumulated up to here. *)
                                  score: int,
                                  commands: Board.command list}

  val toascii : PieceLocation -> string
  val compare : PieceLocation * PieceLocation -> order

  val accessible_locations : Board.state -> PieceLocation list

  val simple_heuristic_solver : (Board.state * ((int * int) -> int) ) -> Board.command list

end
