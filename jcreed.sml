structure Jcreed =
struct

val tagp = Params.param "better_default_tag"
                        (SOME("-tag", "Value to put in the tag field."))
                        "tag"

val problemp = Params.param "15"
                            (SOME("-problem", "Problem number to load."))
                            ("problem")

val timelimitp = Params.param "10"
                              (SOME("-timelimit", "Max number of seconds to spend."))
                              ("timelimit")


fun main () =
  let
    val power_phrases = Phrases.power
    val power_phrases = ["the deep ones"]

    val Pathfind.PS {stream_state=init_stream_state, query} =
        Pathfind.PowerHeuristics.robin power_phrases

    fun lchrs_for_step state stream_state (LockStep.Step {px, py, a, commands, ...}) =
      let
        val powa = Pathfind.find_with_power state
                                            (Pathfind.Target {px=px, py=py, a=a})
                                            (Pathfind.PS { stream_state = stream_state,
                                                           query = query })
        val lock_lchr = Board.anychar (hd commands)
      in
        case powa of SOME (x, stream_state') => (x @ [lock_lchr], stream_state')
                   | NONE => (* shouldn't happen, but just in case... *)
                     (map Board.anychar (rev commands), stream_state)
      end




    fun steps_to_lchrs state ss [] = raise Match
      | steps_to_lchrs state ss ((step as LockStep.Step {state=next_state, ...})::tl) =
        let
          val (this, ss') = lchrs_for_step state ss step
          val rest = (case next_state of
                          SOME state' => steps_to_lchrs state' ss' tl
                        | NONE => [])
        in
          this @ rest
        end

    fun do_seed (problemId, problem, seed_idx, seed) =
      let
        val state = Board.reset (problem, seed_idx)
        val heuristic = LockStep.simple_heuristic problem
        val seconds = Params.asint 3 timelimitp
        val steps = rev (LockStep.play_to_end (state, heuristic, Time.fromSeconds (IntInf.fromInt seconds)))
        val lchrs = steps_to_lchrs state init_stream_state steps
      in
        (if seed_idx > 0
         then print ",\n"
         else ());
        print "{\n";
        print ("\"problemId\": " ^ Int.toString problemId ^ ",\n");
        print ("\"seed\": " ^ Int.toString (Word32.toInt seed) ^ ",\n");
        print ("\"tag\": \"" ^ (!tagp) ^ "\",\n");
        print ("\"solution\": \"");
        print (implode (List.map (Board.forgetlegal) lchrs));
        print "\"\n";
        print "}\n"
      end
    val problemId = Params.asint 1 problemp
    val problem = Board.fromjson
                      ("qualifiers/problem_" ^ Int.toString problemId ^".json")

    val seeds = Board.seeds problem
  in
     print "[\n";
     Vector.appi (fn  (idx, seed) => do_seed (problemId, problem, idx, seed)) seeds;
     print "]\n"

  end
  handle Board.Board s =>
         TextIO.output (TextIO.stdErr, "Uncaught Board: " ^ s ^ "\n")

end

val () = Params.main0 "No arguments." Jcreed.main


                      (*
(* relatively naive greedy search *)

    fun @@ (EQUAL, o2) = o2
      | @@ (o1, _) = o1
    infixr 3 @@


    fun opt_compare (SOME _, SOME _) = EQUAL
      | opt_compare (SOME _, NONE) = GREATER
      | opt_compare (NONE, SOME _) = LESS
      | opt_compare (NONE, NONE) = EQUAL

    fun heur (LockStep.Step s1, LockStep.Step s2) =
      opt_compare (#state s1, #state s2) @@
      Int.compare (#scored s1, #scored s2) @@
      Int.compare (#py s1, #py s2) @@
      Int.compare (#px s1, #px s2)

    fun play_to_end stream_state (state, heur) =
      let
        val nexts = LockStep.possible_next_steps state
        val best_step as LockStep.Step best = ListUtil.max heur nexts
        val (lchrs, stream_state') = lchrs_for_step state stream_state best_step
      in
        lchrs @
        (case (#state best) of
             NONE => []
           | SOME s => play_to_end stream_state' (s, heur))
      end

(*     val lchrs = play_to_end (state, heur) *)
*)
