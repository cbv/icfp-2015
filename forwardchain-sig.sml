signature FORWARD_CHAIN = sig
  datatype PieceLocation = PL of {px: int, py: int, a: int, locked: bool}

  val toascii : PieceLocation -> string
  val compare : PieceLocation * PieceLocation -> order

  val accessible_locations : Board.state -> PieceLocation list

end
