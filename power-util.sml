structure PowerUtil :> POWER_UTIL =
struct

  exception PowerUtil of string

  fun loadproblem p = Board.fromjson
    (StringUtil.readfile ("qualifiers/problem_" ^ Int.toString p ^ ".json"))

  fun problems () =
    let
      val () = print "loading problems...\n"
      val problems = Vector.tabulate(25, loadproblem)
      val () = print "ok.\n"
    in
      problems
    end

  (* Return the longest prefix that does not result in ERROR; can be 0 *)
  fun get_valid_prefix problem seed solution =
    let
      val state = Board.resetwithseed (problem, seed)
      val solution_size = String.size solution
      fun score_from i =
        if i = solution_size
        then i
        else
          let
            val c = Board.legalize (String.sub (solution, i))
            val Board.M { status, ... } = Board.move (state, c)
          in
            case status of
              Board.CONTINUE => score_from (i + 1)
            | Board.GAMEOVER _ => i
            | Board.ERROR => i
          end
  in
    score_from 0
  end

  fun longest_prefix problems solution =
    let
      val best = ref NONE
      fun oneproblem (idx, p) =
        let
          val seeds = Board.seeds p
          fun oneseed s =
            let
              val n = get_valid_prefix p s solution
            in
              if (case !best of
                    NONE => true
                  | SOME (x, _, _) => n > x)
              then best := SOME (n, idx, s)
              else ()
            end
        in
          Vector.app oneseed seeds
        end
    in
      Vector.appi oneproblem problems;
      case !best of
        NONE => raise PowerUtil "There is at least one problem, right?"
      | SOME (n, idx, s) => (n, idx, s)
    end


end
