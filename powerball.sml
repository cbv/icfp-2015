
structure PB =
struct

  exception PB of string
  structure PU = PowerUtil

  val problems = PowerUtil.problems ()

  val guesses = Script.linesfromfile "powerball.txt"
  val guesses = map (StringUtil.losespecsides StringUtil.whitespec) guesses
  val guesses = List.filter
    (fn "" => false
  | s => not (StringUtil.matchhead "#" s)) guesses
  val guesses = map StringUtil.lcase guesses

  structure SS = SplaySetFn(type ord_key = string
                            val compare = String.compare)
  fun deduplicate sl =
    let val s = foldr SS.add' SS.empty sl
    in SS.foldr op:: nil s
    end

  val guesses = deduplicate guesses

  fun filterout s =
    if PU.is_known s
    then
      let in
        print ("Already known: [" ^ s ^ "]\n");
        NONE
      end
    else
      if PU.is_excluded s
      then
        let in
          print ("Already excluded: [" ^ s ^ "]\n");
          NONE
        end
      else
        if PU.is_invalid s
        then
          let in
            print ("Contains invalid chars: [" ^ s ^ "]\n");
            NONE
          end
        else SOME s

  val guesses = List.mapPartial filterout guesses

  (* val () = app (fn s => print (s ^ "\n")) guesses *)

  (* "shotgun" approach just tries to jam a lot of words into
     experiments. *)
  local
    datatype command = datatype Board.command
    datatype dir = datatype Board.dir
                   datatype turn = datatype Board.turn
  in
    val moves = map Board.anychar [(D SE), (D SW),
                                   (D E),  (D W),
                                   (T CW), (T CCW)]
  end


  fun make_experiment problem_idx seed guesses =
    let
      val problem = Vector.sub(problems, problem_idx)
      val state = Board.resetwithseed (problem, seed)

      fun loop (sofar : string, phrases) =
        (* try to insert a power phrase here. *)
        let
          (* Get a phrase that we can insert here, and the
             remaining phrases (possibly reordered) *)
          fun getphrase (_, nil) = NONE
            | getphrase (acc, ph :: rest) =
            if PU.can_execute state ph
               (* Might accidentally make a power word by concatenation;
                  this makes a bad experiment so don't do it *)
               andalso (not (PU.contains_known (sofar ^ ph)))
            then
                SOME (ph, acc @ rest)

            else getphrase (ph :: acc, rest)
        in
          case getphrase (nil, phrases) of
            NONE => (* XXX Explore some... *) (sofar, phrases)
          | SOME (ph, rest) =>
              let in
                PU.execute state ph;
                loop (sofar ^ ph, rest)
              end
        end
    in
      loop ("", guesses)
    end

  fun make_experiments nil =
    print "Got 'em all!\n"
    | make_experiments guesses =
    let
      (* XXX in all states.. *)
      val problem_idx = 24
      val seed : Word32.word = 0w18
    in
      case make_experiment 24 0w18 guesses of
        ("", _) => print ("Leftover guesses: " ^
                          Int.toString (length guesses) ^ "\n")
      | (ph, guesses) =>
          let in
            TextIO.output
            (TextIO.stdOut,
             "./submitty.py --prob " ^ Int.toString problem_idx ^
             " --seed " ^ Int.toString (Word32.toInt seed) ^
             " --tag pb_" ^
             " --sol '" ^ PU.escape ph ^ "'\n");
            make_experiments guesses
          end
    end

  val () = make_experiments guesses
end
