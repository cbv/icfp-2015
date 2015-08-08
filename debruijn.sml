
structure DB =
struct

  val chars = Vector.map Board.forgetlegal Board.legalchars
  val radix = Vector.length chars

  val l = DeBruijnSequence.debruijn (3, radix)
  val s : string = CharVector.fromList (map (fn idx => Vector.sub(chars, idx)) l)

  val () = print (s ^ "\n")

end
