structure David =
struct

  val tagp = Params.param "better_default_tag"
    (SOME("-tag", "Value to put in the tag field."))
    "tag"

  val problemp = Params.param "14"
    (SOME("-problem", "Problem number to load."))
    ("problem")


  val timelimitp = Params.param "10"
    (SOME("-timelimit", "Max number of seconds to spend."))
    ("timelimit")

(* favor far-down squares *)
fun heuristic (x, y) = y

(*
fun do_seed (problemId, problem, seed_idx, seed) =
  let
     val state = Board.reset (problem, seed_idx)
     val commands = ForwardChain.simple_heuristic_solver (state, heuristic)
  in
   (if seed_idx > 0
   then print ",\n"
   else ());
   print "{\n";
   print ("\"problemId\": " ^ Int.toString problemId ^ ",\n");
   print ("\"seed\": " ^ Int.toString (Word32.toInt seed) ^ ",\n");
   print ("\"tag\": \"" ^ (!tagp) ^ "\",\n");
   print ("\"solution\": \"");
   print (implode (List.map (Board.forgetlegal o Board.anychar) commands));
   print "\"\n";
   print "}\n"
  end
*)




fun do_seed (problemId, problem, seed_idx, seed) =
  let
     val state = Board.reset (problem, seed_idx)
     val heuristic = LockStep.simple_heuristic problem
     val seconds = Params.asint 10 timelimitp
     val steps = LockStep.play_to_end (state, heuristic, Time.fromSeconds (IntInf.fromInt seconds))
     val commands = List.rev (List.concat
                                  (List.map
                                       (fn (LockStep.Step {commands, ...}) => commands)
                                       steps))
  in
   (if seed_idx > 0
   then print ",\n"
   else ());
   print "{\n";
   print ("\"problemId\": " ^ Int.toString problemId ^ ",\n");
   print ("\"seed\": " ^ Int.toString (Word32.toInt seed) ^ ",\n");
   print ("\"tag\": \"" ^ (!tagp) ^ "\",\n");
   print ("\"solution\": \"");
   print (implode (List.map (Board.forgetlegal o Board.anychar) commands));
   print "\"\n";
   print "}\n"
  end



fun main () =
  let
    val problemId = Params.asint 1 problemp
    val problem = Board.fromjson
          (StringUtil.readfile
           ("qualifiers/problem_" ^ Int.toString problemId ^".json"))

      val seeds = Board.seeds problem

  in
     print "[\n";
     Vector.appi (fn  (idx, seed) => do_seed (problemId, problem, idx, seed)) seeds;
     print "]\n"
  end
  handle Board.Board s =>
    TextIO.output (TextIO.stdErr, "Uncaught Board: " ^ s ^ "\n")
end


val () = Params.main0 "No arguments." David.main
