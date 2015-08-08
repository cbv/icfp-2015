structure Pathfind :> PATHFIND =
struct
  datatype target = Target of {px: int, py: int, a: int}

  fun cmds_to_string cmds = StringUtil.delimit "-" (List.map (Board.commandstring) cmds)

  datatype ptpos = PTP of {ux:int, uy:int, a:int}
  datatype command = datatype Board.command
  datatype dir = datatype Board.dir
  datatype turn = datatype Board.turn

  val ptz = PTP {ux=0, uy=0, a=0}
  fun ptadd (PTP p, PTP q) =
    PTP { ux = #ux p + #ux q,
          uy = #uy p + #uy q,
          a = (#a p + #a q) mod 6 }

  fun ptsub (PTP p, PTP q) =
    PTP { ux = #ux p - #ux q,
          uy = #uy p - #uy q,
          a = (#a p - #a q) mod 6 }

  fun ptsum pts = foldr ptadd ptz pts

  fun choice_order (PTP {ux,uy,a}) =
    let
(*      val _ = print( (Int.toString ux) ^ " " ^ (Int.toString uy) ^ " " ^ (Int.toString a) ^ "\n") *)
      val (best_turn, worst_turn) =
          if a < 3
          then (T CW, T CCW)
          else (T CCW, T CW)
      val east = 2 * ux + uy > 0
      val horiz =
          if east
          then [D E, D W]
          else [D W, D E]
      val diag =
          if east
          then [D SE, D SW]
          else [D SW, D SE]
      val moves =
          if uy > 0
          then diag @ horiz
          else horiz @ diag
    in
      if (a <> 0)
      then [best_turn] @ moves @ [worst_turn]
      else moves @ [best_turn, worst_turn]
    end

  fun choice_order_for state target =
    let
      val unif = Board.uniformize_coord
      val ((px, py), pa) = (Board.piece_position state, Board.piece_angle state)
      val (px, py) = unif (px, py)
      (* PERF: unnecessary recomputation of uniformization of target *)
      val {px=tx,py=ty,a=ta} = target
      val (tx, ty) = unif (tx, ty)
      val answer = choice_order (ptsub (PTP {ux=tx,uy=ty,a=ta}, PTP {ux=px,uy=py,a=pa}))
    in
(*      print (">" ^ cmds_to_string answer ^ "\n"); *)
      answer
    end

  datatype PieceLocation = PL of {px: int, py: int, a: int,
                                  commands: Board.command list (* TODO(perf) track this somewhere else? *)}

  fun toascii (PL {px, py, a, ...}) =
    "{(" ^ (Int.toString px) ^ ", " ^ (Int.toString py) ^ ") a:" ^ (Int.toString a) ^ "}"

  fun piece_location (state, sym, commands) =
    let
      val ((px, py), angle) = (Board.piece_position state, Board.piece_angle state)
    in
        PL {px = px, py = py, a = angle mod sym, commands = commands}
    end

  fun compare (PL {px = px0, py = py0, a = a0, ...},
               PL {px = px1, py = py1, a = a1, ...}) =
    case Int.compare (px0, px1) of
      EQUAL => (case Int.compare (py0, py1) of
                  EQUAL => Int.compare (a0, a1)
                | other => other)
    | other => other

  structure LocSet = SplaySetFn(struct
                                   type ord_key = PieceLocation
                                   val compare = compare
                                end)

  fun find_first f [] = NONE
    | find_first f (x::tl) =
      (
        (*      print ("find_first " ^ Board.commandstring (Board.charcommand x) ^ "\n"); *)
        (case f x of NONE => find_first f tl
                   | y => y))

  fun helper (state, visitedSetRef, commands, target) =
    let
      val moves = map Board.anychar (choice_order_for state target)
      fun move_helper commands move =
        let
          val sym = Board.piece_symmetry state
          fun body (Board.M {scored, lines, locked, status}) =
            (case status of
                 Board.CONTINUE =>
                 let val new_commands = (Board.charcommand move)::commands
                     val pl = piece_location(state, sym, new_commands)
                     val {px=tx,py=ty,a=ta} = target
                     val ((px, py), pa) = (Board.piece_position state, Board.piece_angle state)
(*
                     val _ = print("piece at " ^ (Int.toString px) ^ " " ^ (Int.toString py) ^ "\n")
                     val _ = print("commands " ^ (cmds_to_string new_commands) ^ "\n")
*)
                 in
                   if py > ty
                   then NONE
                   else if (pa, px, py) = (ta, tx, ty)
                   then SOME new_commands
                   else if LocSet.member (!visitedSetRef, pl)
                   then NONE (* already visited *)
                   else
                     let
                     in
                       visitedSetRef := (LocSet.add (!visitedSetRef, pl));
                       case locked of
                           NONE => helper (state, visitedSetRef, new_commands, target)
                        |  SOME _ => NONE
                     end
                 end
               | _ => NONE
            )
        in
          Board.move_unwind (state, move, body)
        end
    in
      find_first (move_helper commands) moves
    end

  fun find state (Target tgt) =
    let val setRef = ref (LocSet.singleton
                              (piece_location (state, Board.piece_symmetry state, [])));
    in
      Option.map rev (helper (state, setRef, [], tgt))
    end

end
