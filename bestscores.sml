structure BestScores =
struct

  val offsetp = Params.param "0"
    (SOME("-offset", "First problem to work on"))
    "offset"

  structure PU = PowerUtil

  val SECONDS = 5

  fun main () =
    let
      val problems = Vector.tabulate (25, PU.loadproblem)

      type result = { sol: string, score: int }

      fun runsol (problemi : int)
                 (seed_idx : int)
                 (solution : string * Solutions.solution) : string *  result =
          let val problem = Vector.sub (problems, problemi)
              val sol = (#2 solution) { seconds = SECONDS, problem = problem,
                                        seed_idx = seed_idx,
                                        power = Phrases.power,
                                        use_stateset = false}
              val seed_value = Vector.sub (Board.seeds problem, seed_idx)
              val score = PU.get_score problem seed_value sol
          in
              (#1 solution, { sol = sol, score = score })
          end

      fun runseed (problem : int) (seed_idx : int) : result =
          let val sols = List.map (runsol problem seed_idx)
                                  (Solutions.submit_solutions)
              val (best_method, { sol = best_sol, score = best_score }) =
                  List.foldl (fn ((i, { sol = sol1, score = score1 }),
                                  (i2, { sol = sol2, score = score2 })) =>
                                 if score1 > score2 then
                                     (i, { sol = sol1, score = score1 })
                                 else
                                     (i2, { sol = sol2, score = score2 }))
                             ("", { sol = "", score = 0 })
                             sols
              fun filename method score =
                  ("tosubmit/" ^
                   Int.toString problem ^ "_" ^
                   Int.toString seed_idx ^ "_" ^ method ^ "_" ^
                   Int.toString score ^ ".txt")
              fun obj_of_sol sol =
              "[{ \"problemId\": " ^ Int.toString problem ^ ",\n" ^
              "\"seed\": " ^
              Int.toString (Word32.toInt (Vector.sub
                                          (Board.seeds (Vector.sub (problems, problem)),
                                           seed_idx))) ^ ",\n" ^
              "\"solution\": \"" ^ sol ^ "\"\n}]"
          in
              (StringUtil.writefile (filename best_method best_score)
                                    (obj_of_sol best_sol);
               print (filename best_method best_score ^ "\n");
               { sol = best_sol, score = best_score })
          end

      fun runprob (problem: int) : result vector =
          let val p = Vector.sub (problems, problem)
          in
              Vector.map (runseed problem)
                         (Vector.tabulate (Vector.length (Board.seeds p),
                                           fn i => i))
          end

      val offset = Params.asint 0 offsetp

      val r = Vector.tabulate ((Vector.length problems) - offset,
                               (fn i => runprob (i + offset)))
    in
        print "\n";
        ()
    end
  handle Board.Board s => print ("Uncaught Board: " ^ s ^ "\n")

end

val () = Params.main0 "No arguments." BestScores.main
