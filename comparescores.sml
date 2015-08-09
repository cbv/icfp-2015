structure CompareScores =
struct

  structure PU = PowerUtil

  fun david (problem, seed_idx) =
    let
      val state = Board.reset (problem, seed_idx)
      val heuristic = LockStep.simple_heuristic problem
      val seconds = 10 (* Params.asint 10 timelimitp *)
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
    let
      (* just run first seed on each problem for now? *)
      val seed_idx = 0
      val problems = Vector.tabulate (24, PU.loadproblem)

      type result = { sol: string, score: int }
      fun maketable driver : result vector =
        let
          fun oneidx i =
            let
              val problem = Vector.sub (problems, i)
              val sol = driver (problem, seed_idx)
              val seed_value = Vector.sub (Board.seeds problem, seed_idx)
              val score = PU.get_score problem seed_value sol
            in
              print ".";
              { sol = sol, score = score }
            end
        in
          Vector.tabulate (Vector.length problems, oneidx)
        end

      val results_david = maketable david

      val table = ["problem", "david"] ::
        List.tabulate (Vector.length problems,
                       fn pidx =>
                       Int.toString pidx ::
                       map (fn res : result vector =>
                            let val { score, ... } = Vector.sub (res, pidx)
                            in Int.toString score
                            end) [results_david])

    in
      print (StringUtil.table 80 table ^ "\n")
    end
  handle Board.Board s => print ("Uncaught Board: " ^ s ^ "\n")

end

val () = Params.main0 "No arguments." CompareScores.main