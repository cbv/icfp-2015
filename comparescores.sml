structure CompareScores =
struct

  structure PU = PowerUtil

  fun david_with_heuristic (problem, seed_idx, heuristic) =
    let
      val state = Board.reset (problem, seed_idx)
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

  fun both_heuristic problem (hi as LockStep.HI { state, ... }) =
    LockStep.simple_heuristic problem hi -
    Board.raggedness_heuristic state

  fun david (problem, seed_idx) =
    david_with_heuristic (problem, seed_idx, LockStep.simple_heuristic problem)

  fun ragged (problem, seed_idx) =
    david_with_heuristic (problem, seed_idx,
                          (fn (LockStep.HI { state, ... }) =>
                           1000 - Board.raggedness_heuristic state))

  fun both (problem, seed_idx) =
    david_with_heuristic (problem, seed_idx, both_heuristic problem)


  fun highfive (problem, seed_idx) =
    let
      val powerstream =
        Pathfind.PowerHeuristics.robin Phrases.power

      val state = Board.reset (problem, seed_idx)
      val heuristic = both_heuristic problem
      val seconds = 10 (* Params.asint 3 timelimitp *)
      val steps = rev (LockStep.play_to_end (state, heuristic, Time.fromSeconds (IntInf.fromInt seconds)))
      val lchrs = PowerThirst.polish state powerstream steps
    in
      implode (List.map Board.forgetlegal lchrs)
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

          val r = Vector.tabulate (Vector.length problems, oneidx)
        in
          print "\n";
          r
        end

      val results_david = maketable david
      val results_ragged = maketable ragged
      val results_both = maketable both
      val results_highfive = maketable highfive

      val table = ["problem", "david", "ragged", "both"] ::
        List.tabulate (Vector.length problems,
                       fn pidx =>
                       Int.toString pidx ::
                       map (fn res : result vector =>
                            let val { score, ... } = Vector.sub (res, pidx)
                            in Int.toString score
                            end) [results_david,
                                  results_ragged,
                                  results_both,
                                  results_both])

    in
      print (StringUtil.table 80 table ^ "\n")
    end
  handle Board.Board s => print ("Uncaught Board: " ^ s ^ "\n")

end

val () = Params.main0 "No arguments." CompareScores.main