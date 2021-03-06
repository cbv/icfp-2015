structure TestBoard =
struct

  val scriptp = Params.param ""
    (SOME("-script", "Script. Backtick inserts next character from script."))
    "script"

  val problemp = Params.param "11"
    (SOME("-problem", "Problem number to load."))
    ("problem")

  val itos = Int.toString

  (* Amazing! use stty -icanon to make this not wait for a return
     character before sending input. It even works on windows
     in cygwin! *)
  fun get_command () = TextIO.input1 TextIO.stdIn

  fun interactive (script, state) =
    let
      fun commandloop () =
        let
          val c = case get_command () of
            NONE => raise Board.Board "No input?"
          | SOME c => c
        in
          print (ANSI.CLS ^ "\n");
          (case c of
             #"`" =>
             (case script of
                nil => raise Board.Board "Script exhausted."
              | h :: t =>
                  let in
                    print ("From script: [" ^ implode [h] ^ "]\n");
                    print ("Which is command: " ^
                           Board.commandstring (Board.charcommand
                                                (Board.legalize h)) ^ "\n");
                    (Board.move (state, Board.legalize h), t)
                  end)
           | #"h" => (Board.move (state, Board.anychar (Board.D Board.W)), script)
           | #"k" => (Board.move (state, Board.anychar (Board.D Board.E)), script)
           | #"m" => (Board.move (state, Board.anychar (Board.D Board.SE)), script)
           | #"n" => (Board.move (state, Board.anychar (Board.D Board.SW)), script)

           | #"y" => (Board.move (state, Board.anychar (Board.T Board.CCW)), script)
           | #"u" => (Board.move (state, Board.anychar (Board.T Board.CW)), script)

           | c =>
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
      val () = print (Board.powerinfostring state ^ "\n")
      val (Board.M { scored, lines, locked, status, new_phrases }, script) =
        commandloop ()
    in
      print ("Pieces left: " ^ itos (Board.piecesleft state) ^ "\n");
      print ("Scored " ^ itos scored ^ " in " ^ itos lines ^ " lines " ^
             " and new phrases: " ^ itos new_phrases ^
             " and locked: " ^ Bool.toString (Option.isSome locked) ^ "\n");
      (case status of
         Board.CONTINUE => interactive (script, state)
       | Board.GAMEOVER Board.COMPLETE => print "COMPLETE.\n"
       | Board.GAMEOVER Board.NO_SPACE => print "NO_SPACE.\n"
       | Board.ERROR => print "ERROR!\n")
    end

  fun main args =
    let
      val problem = Board.fromjson
                        ("qualifiers/problem_" ^ !problemp ^ ".json")

      (* XXX add a command line parameter for the seed?  how does that work..
         if it's optional.. *)
      val state = Board.reset (problem, 0)
    in
      print "There is nothing, only Zuulthuhu.\n";
      interactive (explode (!scriptp), state)
    end
  handle Board.Board s =>
    TextIO.output (TextIO.stdErr, "Uncaught Board: " ^ s ^ "\n")

  (* Note: SML/NJ is not processing any Params at all. *)
  fun smlnj_entry (name, args) =
      let val _ = main args
      in 0
      end

end
