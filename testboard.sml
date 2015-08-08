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

  fun get_command () = TextIO.input1 TextIO.stdIn

  fun interactive state =
    let
      val () = print (ANSI.CLS ^ "\n")
      fun commandloop () =
        (* Amazing! use stty -icanon to make this not wait for a return
           character before sending input. It even works on windows
           in cygwin! *)
        (case get_command() of
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
      print (Board.toascii state ^ "\n");
      commandloop ();
      interactive state
    end

  fun main args =
    let
      val problem = Board.fromjson
        (StringUtil.readfile "qualifiers/problem_17.json")

      val state = Board.reset (problem, 0)
    in
      print "There is nothing, only Zuulthuhu.\n";
      interactive state
    end
  handle Board.Board s =>
    TextIO.output (TextIO.stdErr, "Uncaught Board: " ^ s ^ "\n")

  fun smlnj_entry (name, args) =
      let val _ = main args
      in 0
      end

end
