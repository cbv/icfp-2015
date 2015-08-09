structure David =
struct

  val tagp = Params.param "better_default_tag"
    (SOME("-tag", "Value to put in the tag field."))
    "tag"

  val problemp = Params.param "14"
    (SOME("-problem", "Problem number to load."))
    ("problem")


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



(* opens for open cells with small y coordinate *)
fun lockstep_heuristic problem state =
  let
      val (width, height) = Board.size problem
      val score = ref 0
      val () = Util.for
                   0 (width - 1)
                   (fn ii => Util.for 0 (height - 1)
                                      (fn jj =>
                                          if Board.isempty (state, ii, jj)
                                          then
                                              let
                                              in
                                                  (* more points, proportional to distance from botton *)
                                                  score := ((!score) + (height - jj) )
                                              end
                                          else ()
                                      ))

  in
      !score
  end

fun do_seed (problemId, problem, seed_idx, seed) =
  let
     val state = Board.reset (problem, seed_idx)
     val heuristic = lockstep_heuristic problem
     val steps = LockStep.play_to_end (state, heuristic)
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
