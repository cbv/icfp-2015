structure TestBoard =
struct

  fun testrng (rng, 0) = ()
    | testrng (rng, n) =
    let
      val (x, rng) = RNG.next rng
    in
      print (Int.toString x ^ "\n");
      testrng (rng, n - 1)
    end

  val () = testrng (RNG.fromseed 0w17, 20)

  fun interactive state =
    let
      fun commandloop () =
        (case TextIO.input1 TextIO.stdIn of
           SOME #"h" => Board.move (state, Board.anychar (Board.D Board.W))
         | SOME #"k" => Board.move (state, Board.anychar (Board.D Board.E))
         | NONE => raise Board.Board "No input??"
         | SOME c =>
           let in
             print ("I don't know char " ^ implode [c] ^ " = #" ^
                    Int.toString (ord c) ^ "\n");
             commandloop()
           end)
    in
      print ANSI.CLS;
      print (Board.toascii state ^ "\n");
      commandloop ();
      interactive state
    end

  fun main () =
    let
      val problem = Board.fromjson
        (StringUtil.readfile "qualifiers/problem_3.json")

      val state = Board.reset (problem, 0)
    in
      print "There is nothing, only Zuulthuhu.\n";
      interactive state
    end
  handle Board.Board s =>
    TextIO.output (TextIO.stdErr, "Uncaught Board: " ^ s ^ "\n")

end

val () = TestBoard.main()