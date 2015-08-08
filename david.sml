structure David =
struct

(* favor far-down squares *)
fun heuristic (x, y) = y

val problemId = 14

fun do_seed (problem, seed_idx, seed) =
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
   print ("\"tag\": \"david_insomniac\",\n");
   print ("\"solution\": \"");
   print (implode (List.map (Board.forgetlegal o Board.anychar) commands));
   print "\"\n";
   print "}\n"
  end

fun main () =
  let
      val problem = Board.fromjson
                        (StringUtil.readfile ("qualifiers/problem_"^ Int.toString problemId ^".json"))

      val seeds = Board.seeds problem

  in
     print "[\n";
     Vector.appi (fn  (idx, seed) => do_seed (problem, idx, seed)) seeds;
     print "]\n"
  end
  handle Board.Board s =>
    TextIO.output (TextIO.stdErr, "Uncaught Board: " ^ s ^ "\n")

val () = main ()

end
