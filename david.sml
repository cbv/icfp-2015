structure David =
struct

(* favor far-down squares *)
fun heuristic (x, y) = y

fun main () =
  let
      val problem = Board.fromjson
                        (StringUtil.readfile ("qualifiers/problem_2.json"))

      val state = Board.reset (problem, 1)
     val commands = ForwardChain.simple_heuristic_solver (state, heuristic)
  in
   print (implode (List.map (Board.forgetlegal o Board.anychar) commands))
  end
  handle Board.Board s =>
    TextIO.output (TextIO.stdErr, "Uncaught Board: " ^ s ^ "\n")

val () = main ()

end
