structure Solutions :> SOLUTIONS =
struct

  type solution = { seconds : int,
                    problem : Board.problem,
                    seed_idx : int,
                    power : string list} -> string

  fun lift_heuristic h (LockStep.HI { state, ... }) = h state

  fun david_with_heuristic heuristic { seconds, problem, seed_idx, ... } =
    let
      val state = Board.reset (problem, seed_idx)
      val steps = LockStep.play_to_end (state, heuristic,
                                        Time.fromSeconds (IntInf.fromInt seconds),
                                        false)
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

  fun positive t = if t <= 0 then 1
                   else t

  fun high_with_endgame engram { seconds, problem, seed_idx, power } =
    let
      val start = Time.now ()
      val time_for_search = positive ((seconds * 3) div 4)
      (* val time_for_polish = positive (seconds - time_for_search) *)

      val powerstream =
        Pathfind.PowerHeuristics.robin engram power

      val state = Board.reset (problem, seed_idx)
      val heuristic = both_heuristic
      val steps = rev (LockStep.play_to_end (state, heuristic,
                                             Time.fromSeconds (IntInf.fromInt seconds), false))
      val after_search = Time.now ()
      val elapsed = IntInf.toInt (Time.toSeconds (Time.-(after_search, start)))
      val remaining = positive (seconds - elapsed)

      val lchrs = PowerThirst.polish remaining state powerstream steps
    in
      implode (List.map Board.forgetlegal lchrs)
    end

  (* TODO: try "ia! ia! " *)
  val highfive = high_with_endgame "ia! ia!"
  val highsix = high_with_endgame "ei!"
  val highseven = high_with_endgame "ia! ia! "

  val all_solutions =
    [("david", david),
     ("ragged", ragged),
     ("both", both),
     ("highfive", highfive)]

  val test_solutions =
    [(* ("david", david),
     ("ragged", ragged),
     ("both", both), *)
     ("highsix", highsix)]

  val submit_solutions =
    [("highseven", highseven),
     ("highfive", highfive),
     ("highsix", highsix)]

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

        and then on jason's laptop:

        problem highfive highsix
        0       6017     6017
        1       1420     1420
        2       8192     9506
        3       6728     7850
        4       14092    17530
        5       3922     3722
        6       7760     7860
        7       4278     4278
        8       8253     8253
        9       5670     5670
        10      4388     4388
        11      4173     4173
        12      6161     6806
        13      3363     3363
        14      20594    30396
        15      5034     6504
        16      7464     8174
        17      2020     2020
        18      8624     11478
        19      1920     1920
        20      5012     5232
        21      926      926
        22      5438     5438
        23      669      669
        24      93386    130380

  *)
  val best_solution = highsix

end
