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

  fun can_execute state str =
    let
      fun exec i =
        if i = size str
        then true
        else
          let
            val c = Board.legalize (String.sub (str, i))
          in
            Board.move_unwind (state, c,
            (fn Board.M { status, ... } =>
             case status of
               Board.CONTINUE => exec (i + 1)
             | Board.GAMEOVER _ => false
             | Board.ERROR => false))
          end
  in
    exec 0
  end

  fun execute state str =
    let
      fun exec i =
        if i = size str
        then ()
        else
          let
            val c = Board.legalize (String.sub (str, i))
            val Board.M { status, ... } = Board.move (state, c)
          in
            case status of
              Board.CONTINUE => exec (i + 1)
            | Board.GAMEOVER _ => raise PowerUtil "It was too long"
            | Board.ERROR => raise PowerUtil "It was ERROR"
          end
  in
    exec 0
  end



  structure SS = SplaySetFn(type ord_key = string
                            val compare = String.compare)

  val all_excluded = map StringUtil.lcase (Excluded.excluded @ Phrases.weakness)
  val known_powerwords = map StringUtil.lcase Phrases.power
  val known_powerwords_set = foldr SS.add' SS.empty known_powerwords

  fun is_excluded s =
    let
      fun try nil = false
        | try (w :: rest) =
        case StringUtil.find s w of
          NONE => try rest
        | SOME _ => true
    in
      try all_excluded
    end

  fun contains_known s =
    List.exists (fn word =>
                 case StringUtil.find word s of
                   NONE => false
                 | SOME _ => true) known_powerwords


  fun is_invalid s =
    CharVector.exists (fn c => not (Board.islegal c)) s

  fun is_known s = SS.member (known_powerwords_set, s)

  (* Ridiculous! Since we are targeting 'quoted' output, turn a single
     quote into ' (ending the quote) "'" (quoted quote) ' (restart quotes).
     Of course, quote all that for SML string literals. *)
  fun escape s = StringUtil.replace "'" "'\"'\"'" s

end
