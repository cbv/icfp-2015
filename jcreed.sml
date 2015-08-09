structure Jcreed =
struct

val problemp = Params.param "15"
                            (SOME("-problem", "Problem number to load."))
                            ("problem")

fun main () =
  let
    val power_phrases = Phrases.power
    val power_phrases = ["john bigboote"]
    val problemId = Params.asint 1 problemp
    val problem = Board.fromjson
                      (StringUtil.readfile
                           ("qualifiers/problem_" ^ Int.toString problemId ^ ".json"))
    val state = (Board.reset (problem, 0))
    (* val _ = *)
    (*     List.app *)
    (*         (fn x => (Board.move (state, (Board.legalize x)); ())) *)
    (*         (explode "ea!dea!pppppk") *)
    (* val ((px, py), pa) = (Board.piece_position state, Board.piece_angle state) *)
    (* val _ = print ("piece at " ^ (Int.toString px) ^ " " ^ (Int.toString py) ^ "\n") *)

    (* fun body (Board.M {scored, lines, locked, new_phrases = _, status}) = *)
    (*   let *)
    (*     val ((px, py), pa) = (Board.piece_position state, Board.piece_angle state) *)
    (*     val _ = print ("inner piece at " ^ (Int.toString px) ^ " " ^ (Int.toString py) ^ "\n") *)
    (*   in *)
    (*     () *)
    (*   end *)
    (* val _ = Board.move_unwind(state, Board.legalize #"b", body) *)

    (* val _ = raise Match *)
    (* val words = *)
    (*     ["ghatanothoa", "hastur", "hypnos", "ithaqua", "nodens", "nyarlathotep"] *)
    (* fun word_to_lock word = *)
    (*   let *)
    (*     val soln = Pathfind.find_with_power *)
    (*                    state *)
    (*                    (Pathfind.Target {px=8, py=18, a=0}) *)
    (*                    (fn n => word) *)
    (*   in *)
    (*     case soln of NONE => "" *)
    (*                | SOME lchrs => *)
    (*                  (implode (List.map (Board.forgetlegal) lchrs)) ^ "a" *)
    (*   end *)
(*    val soln = Powerize.insert (Board.reset (problem, 0))
                               (Pathfind.Target {px=7, py=13, a=5})
                               "ea! ea! ea!" *)
    fun @@ (EQUAL, o2) = o2
      | @@ (o1, _) = o1
    infixr 3 @@
    val nn = ref 0
    val stream = (fn n =>
                     let val w = List.nth(power_phrases, !nn)
                         val _ = nn := (!nn + 1) mod (length power_phrases)
                     in
                       w
                     end)
    fun lchrs_for_step state {px, py, a, commands, ...} =
      let
        val powa = Pathfind.find_with_power state
                                            (Pathfind.Target {px=px, py=py, a=a})
                                            stream
        val lock_lchr = Board.anychar (hd commands)
      in
        case powa of SOME x => x @ [lock_lchr]
                   | NONE => (* shouldn't happen, but just in case... *)
                     map Board.anychar (rev commands)
      end

    fun opt_compare (SOME _, SOME _) = EQUAL
      | opt_compare (SOME _, NONE) = GREATER
      | opt_compare (NONE, SOME _) = LESS
      | opt_compare (NONE, NONE) = EQUAL

    fun heur (LockStep.Step s1, LockStep.Step s2) =
      opt_compare (#state s1, #state s2) @@
      Int.compare (#scored s1, #scored s2) @@
      Int.compare (#py s1, #py s2) @@
      Int.compare (#px s1, #px s2)

    fun play_to_end (state, heur) =
      let
        val nexts = LockStep.possible_next_steps state
        val LockStep.Step best = ListUtil.max heur nexts
      in
        lchrs_for_step state best @
        (case (#state best) of
             NONE => []
           | SOME s => play_to_end (s, heur))
      end

    val lchrs = play_to_end (state, heur)
  in
    print (implode (map Board.forgetlegal lchrs))
  end
  handle Board.Board s =>
         TextIO.output (TextIO.stdErr, "Uncaught Board: " ^ s ^ "\n")

end

val () = Params.main0 "No arguments." Jcreed.main
