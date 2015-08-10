structure Driver =
struct

  val inputfile = Params.param ""
    (SOME("-f", "File containing JSON encoded input"))
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

  fun main () =
      let val problem = Board.fromjson (!inputfile)
          val time = Int.div (Params.asint 60 timep,
                              Vector.length (Board.seeds problem))
          val mem = Params.asint 1024 memp
          fun obj_of_sol sol seed_idx =
              "{ \"problemId\": " ^ Int.toString (Board.id problem) ^ ",\n" ^
              "\"seed\": " ^
              Word.toString (Vector.sub (Board.seeds problem, seed_idx)) ^ ",\n" ^
              "\"solution\": \"" ^ sol ^ "\"\n}"
          val s =
              Vector.foldli (fn (seed_idx, _, s) =>
                        let val sol =
                                Solutions.best_solution {seconds = time,
                                                         problem = problem,
                                                         seed_idx = seed_idx,
                                                         power = !power}
                        in
                            s ^ obj_of_sol sol seed_idx ^ ",\n"
                        end)
                    "["
                    (Board.seeds problem)
          val s' = String.substring (s, 0, (String.size s) - 2) ^ "]"
      in
          print s'
      end
end

val () = Params.main0 "Takes no arguments" Driver.main
