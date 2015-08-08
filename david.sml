structure David =
struct

fun main () =
  let
      val problem = Board.fromjson
                        (StringUtil.readfile ("qualifiers/problem_17.json"))

      val state = Board.reset (problem, 0)
   val commands = ForwardChain.simple_heuristic_solver state
  in
   print (implode (List.map (Board.forgetlegal o Board.anychar) commands))
  end

val () = main ()

end
