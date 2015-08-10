structure Solutions :> SOLUTIONS =
struct

  type solution = { seconds : int,
                    problem : Board.problem,
                    seed_idx : int } -> string

  fun lift_heuristic h (LockStep.HI { state, ... }) = h state

  fun david_with_heuristic heuristic { seconds, problem, seed_idx } =
    let
      val state = Board.reset (problem, seed_idx)
      val steps = LockStep.play_to_end (state, heuristic,
                                        Time.fromSeconds (IntInf.fromInt seconds))
      val commands = List.rev (List.concat
                               (List.map
                                (fn (LockStep.Step {commands, ...}) => commands)
                                steps))
    in
      implode (List.map (Board.forgetlegal o Board.anychar) commands)
    end

  fun both_heuristic (hi as LockStep.HI { state, ... }) =
    Board.simple_heuristic state -
    Board.raggedness_heuristic state

  fun david config =
    david_with_heuristic (lift_heuristic Board.simple_heuristic) config

  fun ragged config =
    david_with_heuristic (fn (LockStep.HI { state, ... }) =>
                          1000 - Board.raggedness_heuristic state) config

  fun both config =
    david_with_heuristic both_heuristic config

  fun highfive { seconds, problem, seed_idx } =
    let
      val powerstream =
        Pathfind.PowerHeuristics.robin "ia! ia!" Phrases.power

      val state = Board.reset (problem, seed_idx)
      val heuristic = both_heuristic
      val steps = rev (LockStep.play_to_end (state, heuristic,
                                             Time.fromSeconds (IntInf.fromInt seconds)))
      val lchrs = PowerThirst.polish state powerstream steps
    in
      implode (List.map Board.forgetlegal lchrs)
    end

  fun highsix { seconds, problem, seed_idx } =
    let
      val powerstream =
        Pathfind.PowerHeuristics.robin "ei!" Phrases.power

      val state = Board.reset (problem, seed_idx)
      val heuristic = both_heuristic
      val steps = rev (LockStep.play_to_end (state, heuristic,
                                             Time.fromSeconds (IntInf.fromInt seconds)))
      val lchrs = PowerThirst.polish state powerstream steps
    in
      implode (List.map Board.forgetlegal lchrs)
    end


  val all_solutions =
    [("david", david),
     ("ragged", ragged),
     ("both", both),
     ("highfive", highfive)]

  val test_solutions =
    [("david", david),
     ("ragged", ragged),
     ("both", both),
     ("highfive", highfive)]

  (* Time notwithstanding, this produces the best scores in our test:

        problem david ragged both highfive
        0       2849  3049   3149 6643
        1       700   900    800  1420
        2       1600  2400   2000 8090
        3       460   719    1060 6544
        4       3744  8700   9900 13172
        5       960   338    1160 6202
        6       4166  4166   4266 7760
        7       460   338    360  4278
        8       6053  6053   6053 8253
        9       936   1127   4112 5998
        10      2400  2300   2400 4918
        11      905   905    1005 4173
        12      715   29     730  6176
        13      170   153    204  3380
        14      992   366    1092 20594
        15      220   220    1008 5992
        16      1834  1734   1934 7492
        17      1400  1400   1400 2020
        18      631   191    1050 8624
        19      1300  1300   1300 1920
        20      1744  1644   1844 5012
        21      620   620    620  926
        22      2600  2600   2600 5438
        23      681   1465   472  1320
     *)
  val best_solution = highfive

end
