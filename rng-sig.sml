signature RNG =
sig

  (* functional *)
  type rng
  val fromseed : Word32.word -> rng
  val next : rng -> int * rng

end
