structure Jcreed =
struct

val problemp = Params.param "15"
                            (SOME("-problem", "Problem number to load."))
                            ("problem")

fun main () =
  let
    val problemId = Params.asint 1 problemp
    val problem = Board.fromjson
                      (StringUtil.readfile
                           ("qualifiers/problem_" ^ Int.toString problemId ^ ".json"))
    val state = (Board.reset (problem, 0))
    (* val _ = *)
    (*     List.app *)
    (*         (fn x => (Board.move (state, (Board.legalize x)); ())) *)
    (*         (explode "ea!dea!pppppk") *)
    (* val ((px, py), pa) = (Board.piece_position state, Board.piece_angle state) *)
    (* val _ = print ("piece at " ^ (Int.toString px) ^ " " ^ (Int.toString py) ^ "\n") *)

    (* fun body (Board.M {scored, lines, locked, new_phrases = _, status}) = *)
    (*   let *)
    (*     val ((px, py), pa) = (Board.piece_position state, Board.piece_angle state) *)
    (*     val _ = print ("inner piece at " ^ (Int.toString px) ^ " " ^ (Int.toString py) ^ "\n") *)
    (*   in *)
    (*     () *)
    (*   end *)
    (* val _ = Board.move_unwind(state, Board.legalize #"b", body) *)

    (* val _ = raise Match *)
    val words =
        ["ghatanothoa", "hastur", "hypnos", "ithaqua", "nodens", "nyarlathotep"]
    fun word_to_lock word =
      let
        val soln = Pathfind.find_with_power
                       state
                       (Pathfind.Target {px=8, py=18, a=0})
                       (fn n => word)
      in
        case soln of NONE => ""
                   | SOME lchrs =>
                     (implode (List.map (Board.forgetlegal) lchrs)) ^ "a"
      end
(*    val soln = Powerize.insert (Board.reset (problem, 0))
                               (Pathfind.Target {px=7, py=13, a=5})
                               "ea! ea! ea!" *)

  in
    print (StringUtil.delimit "\n" (map word_to_lock words))
  end
  handle Board.Board s =>
         TextIO.output (TextIO.stdErr, "Uncaught Board: " ^ s ^ "\n")

end

val () = Params.main0 "No arguments." Jcreed.main
