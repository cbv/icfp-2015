structure TestBoard =
struct

  val itos = Int.toString

  fun testrng (rng, 0) = ()
    | testrng (rng, n) =
    let
      val (x, rng) = RNG.next rng
    in
      print (itos x ^ "\n");
      testrng (rng, n - 1)
    end

  val () = testrng (RNG.fromseed 0w17, 20)

  (* Amazing! use stty -icanon to make this not wait for a return
     character before sending input. It even works on windows
     in cygwin! *)
  fun get_command () = TextIO.input1 TextIO.stdIn

  fun interactive state =
    let
      fun commandloop () =
        let val c = get_command ()
        in
          print (ANSI.CLS ^ "\n");
          (case c of
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
                          itos (ord c) ^ "\n");
                   commandloop()
                 end)
        end
      val () = print (Board.toascii state ^ "\n")
      val Board.M { scored, lines, locked, status } = commandloop()
    in
      print ("Scored " ^ itos scored ^ " in " ^ itos lines ^ " lines " ^
             " and locked: " ^ Bool.toString locked ^ "\n");
      (case status of
         Board.CONTINUE => interactive state
       | Board.COMPLETE => print "COMPLETE.\n"
       | Board.NO_SPACE => print "NO_SPACE.\n"
       | Board.ERROR => print "ERROR!\n")
    end

  fun main args =
    let
      val problem = Board.fromjson
        (StringUtil.readfile "qualifiers/problem_11.json")

      val state = Board.reset (problem, 0)
      (* val commands = ForwardChain.simple_heuristic_solver state *)
    in
      print "There is nothing, only Zuulthuhu.\n";
      interactive state
(*      print (implode (List.map (Board.forgetlegal o Board.anychar) commands)) *)
    end
  handle Board.Board s =>
    TextIO.output (TextIO.stdErr, "Uncaught Board: " ^ s ^ "\n")

  fun smlnj_entry (name, args) =
      let val _ = main args
      in 0
      end

end
