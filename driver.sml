structure Driver =
struct

  val inputfiles = Params.paramacc []
    (SOME("-f", "File containing JSON encoded input", #","))
    "file"

  val timep = Params.param "60"
    (SOME("-t", "Time limit, in seconds to produce output"))
    "timeout"

  val memp = Params.param "1024"
    (SOME("-m", "Memory limit, in megabytes, to produce output"))
    "mem"

  val cores = Params.param "1"
    (SOME("-c", "Number of processor cores available"))
    "cores"

  val power = Params.paramacc []
    (SOME("-p", "Phrase of power", #","))
    "phrase"

  fun solve_problem time file =
      let val problem = Board.fromjson file
          fun obj_of_sol sol seed_idx =
              "{ \"problemId\": " ^ Int.toString (Board.id problem) ^ ",\n" ^
              "\"seed\": " ^
              Int.toString (Word32.toInt (Vector.sub
                                          (Board.seeds problem,
                                           seed_idx))) ^ ",\n" ^
              "\"tag\": \"CBV\",\n" ^
              "\"solution\": \"" ^ sol ^ "\"\n}"
      in
              Vector.foldli (fn (seed_idx, _, s) =>
                        let val sol =
                                Solutions.best_solution {seconds = time,
                                                         problem = problem,
                                                         seed_idx = seed_idx,
                                                         power = !power}
                        in
                            s ^ obj_of_sol sol seed_idx ^ ",\n"
                        end)
                    ""
                    (Board.seeds problem)
      end

  fun main () =
    case !inputfiles of
      nil => print "No input files.\n"
    | _ =>
        let
          fun seeds_for_prob file =
            Vector.length (Board.seeds (Board.fromjson file))
          val seeds = List.foldl op+ 0 (List.map seeds_for_prob (!inputfiles))
          val time = Int.div (Params.asint 60 timep, seeds)
          val sols = List.map (solve_problem time) (!inputfiles)
          val s = "[" ^ String.concat sols
          val s' = String.substring (s, 0, (String.size s) - 2) ^ "]"
        in
          print s'
        end
end

val () = Params.main0 "Takes no arguments" Driver.main
