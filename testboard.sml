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
        (StringUtil.readfile "qualifiers/problem_3.json")

      val state = Board.reset (problem, 0)
    in
      print ("Problem:\n" ^ Board.toascii state ^ "\n");
      print "There is nothing, only Zuulthuhu.\n";
      loop (RNG.fromseed 0w17, 20)
    end
  handle Board.Board s =>
    TextIO.output (TextIO.stdErr, "Uncaught Board: " ^ s ^ "\n")

end

val () = TestBoard.main()