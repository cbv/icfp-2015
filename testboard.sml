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
        (* Amazing! use stty -icanon to make this not wait for a return
           character before sending input. It even works on windows
           in cygwin! *)
        (case TextIO.input1 TextIO.stdIn of
           SOME #"h" => Board.move (state, Board.anychar (Board.D Board.W))
         | SOME #"k" => Board.move (state, Board.anychar (Board.D Board.E))
         | SOME #"m" => Board.move (state, Board.anychar (Board.D Board.SE))
         | SOME #"n" => Board.move (state, Board.anychar (Board.D Board.SW))

         | SOME #"y" => Board.move (state, Board.anychar (Board.T Board.CCW))
         | SOME #"u" => Board.move (state, Board.anychar (Board.T Board.CW))

         | NONE => raise Board.Board "No input??"
         | SOME c =>
             if ord c = 3
             then raise Board.Board "exit for interrupt."
             else
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
        (StringUtil.readfile "qualifiers/problem_4.json")

      val state = Board.reset (problem, 0)
    in
      print "There is nothing, only Zuulthuhu.\n";
      interactive state
    end
  handle Board.Board s =>
    TextIO.output (TextIO.stdErr, "Uncaught Board: " ^ s ^ "\n")

end

val () = TestBoard.main()