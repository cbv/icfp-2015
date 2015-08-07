structure RNG :> RNG =
struct

  (*
    bits:
    33222222222211111111110000000000
    10987654321098765432109876543210
     ^^^^^^^^^^^^^^^
    *)


  (* Modulus is implicit; using word32 *)
  val MULTIPLIER : Word32.word = 0w1103515245
  val INCREMENT : Word32.word = 0w12345

  type rng = Word32.word
  fun fromseed (x : Word32.word) : rng = x
  fun next (seed : rng) =
    let
      val bits = Word32.>> (seed, 0w16)
      (* 2^15 - 1 *)
      val bits = Word32.andb (bits, 0w32767)
      val seed' = seed * MULTIPLIER + INCREMENT
    in
      (Word32.toInt bits, seed')
    end

end
