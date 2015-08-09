structure CompareScores =
struct

  fun david (problem, seed_idx) =
    let
      val state = Board.reset (problem, seed_idx)
      val heuristic = LockStep.simple_heuristic problem
      val seconds = Params.asint 10 timelimitp
      val steps = LockStep.play_to_end (state, heuristic,
                                        Time.fromSeconds (IntInf.fromInt seconds))
      val commands = List.rev (List.concat
                               (List.map
                                (fn (LockStep.Step {commands, ...}) => commands)
                                steps))
    in
      implode (List.map (Board.forgetlegal o Board.anychar) commands)
    end


  fun main () =
    (* just run first seed on each problem for now? *)
    let
      val problems = Vector.tabulate (24, fn i =>
                                     Board.fromjson
                                     ("qualifiers/problem_" ^ Int.toString problemId ^".json"))

      fun maketable driver =
        let
          fun oneidx i =
            let
              val problem = Vector.sub (problems, i)
              val sol = driver (problem, 0)
              val score = PU.score problem sol
            in
              print ".";
              { sol = sol, score = score }
            end
        in
          Vector.tabulate (Vector.size problems, oneidx)
        end

      val results_david = maketable david

      val table = [" ", "david"] ::
        List.tabulate (fn pidx =>
                       Int.toString pidx ::
                       map (fn res =>
                            let val { score, ... } = Vector.sub (res, pidx)
                            in Int.toString score
                            end) [results_david])

    in
      print (StringUtil.table 80 table ^ "\n")
    end
  handle Board.Board s => print ("Uncaught Board: " ^ s ^ "\n")

end

val () = Params.main0 "No arguments." CompareScores.main