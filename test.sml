
(* old dead dry dust *)

  fun testrng (rng, 0) = ()
    | testrng (rng, n) =
    let
      val (x, rng) = RNG.next rng
    in
      print (itos x ^ "\n");
      testrng (rng, n - 1)
    end

  val () = testrng (RNG.fromseed 0w17, 20)
