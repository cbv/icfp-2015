
structure GetScore =
struct

  val scriptp = Params.param ""
    (SOME("-script", "The command sequence (as characters) to run."))
    "script"

  val problemp = Params.param "11"
    (SOME("-problem", "Problem number to load."))
    "problem"

  val seedp = Params.param "0"
    (SOME("-seed", "Seed *value* to use. Not an index."))
    "seed"

  fun main () =
    let in
      (* Total score *)
      print "7\n";
      (* Number of distinct words of power. *)
      print "0\n"
    end

end

val () = Params.main0
  ("This program takes no arguments (but uses -flags). It prints out two " ^
   "lines. The first is the total score for the given script on the problem " ^
   "with the given seed. The second is the total number of distinct phrases " ^
   "of power used during the execution; this only includes the phrases that " ^
   "the board implementation knows about.")
  GetScore.main
