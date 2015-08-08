structure Jcreed =
struct

val problemp = Params.param "15"
                            (SOME("-problem", "Problem number to load."))
                            ("problem")

fun main () =
  let
    val problemId = Params.asint 1 problemp
    val problem = Board.fromjson
                      (StringUtil.readfile
                           ("qualifiers/problem_" ^ Int.toString problemId ^ ".json"))
    val soln = Pathfind.find (Board.reset (problem, 0))
                             (Pathfind.Target {px=7, py=13, a=1})
  in
    case soln of NONE => print "None!\n"
               | SOME cmds =>
                 print (implode (List.map (Board.forgetlegal o Board.anychar) cmds))
  end
  handle Board.Board s =>
         TextIO.output (TextIO.stdErr, "Uncaught Board: " ^ s ^ "\n")

end

val () = Params.main0 "No arguments." Jcreed.main
