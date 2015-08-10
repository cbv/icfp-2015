structure CompareScores =
struct

  structure PU = PowerUtil

  fun main () =
    let
      (* just run first seed on each problem for now? *)
      val seed_idx = 0
      val problems = Vector.tabulate (25, PU.loadproblem)

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

      val results = ListUtil.mapsecond maketable Solutions.all_solutions

      val table = ("problem" :: map #1 results) ::
        List.tabulate (Vector.length problems,
                       fn pidx =>
                       Int.toString pidx ::
                       map (fn res : result vector =>
                            let val { score, ... } = Vector.sub (res, pidx)
                            in Int.toString score
                            end) (map #2 results))

    in
      print (StringUtil.table 80 table ^ "\n")
    end
  handle Board.Board s => print ("Uncaught Board: " ^ s ^ "\n")

end

val () = Params.main0 "No arguments." CompareScores.main