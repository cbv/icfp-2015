structure TestBoard =
struct

  fun loop (rng, 0) = ()
    | loop (rng, n) =
    let
      val (x, rng) = RNG.next rng
    in
      print (Int.toString x ^ "\n");
      loop (rng, n - 1)
    end

  fun main() =
    let
      val problem = Board.fromjson
        (StringUtil.readfile "qualifiers/problem_11.json")
    in
      print "There is nothing, only Zuulthuhu.\n";
      loop (RNG.fromseed 0w17, 20)
    end

end

val () = TestBoard.main()