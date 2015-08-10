structure Pathfind :> PATHFIND =
struct
  datatype 'a power_stream = PS of {stream_state: 'a,
                                    query: 'a -> string * 'a * 'a}

  datatype target = Target of {px: int, py: int, a: int}

  fun cmds_to_string cmds = StringUtil.delimit "-" (List.map (Board.commandstring) cmds)
  fun lchrs_to_string cmds = implode (map Board.forgetlegal cmds) ^ " (" ^ (StringUtil.delimit "-" (List.map (Board.commandstring o Board.charcommand) cmds)) ^ ")"

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
      (* val _ = print( (Int.toString ux) ^ " " ^ (Int.toString uy) ^ " " ^ (Int.toString a) ^ "\n") *)
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
      (* print (">" ^ cmds_to_string answer ^ "\n"); *)
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
          fun body (Board.M {scored, lines, locked, new_phrases = _, status}) =
            (case (locked, status) of
                 (NONE, Board.CONTINUE) =>
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
                       helper (state, visitedSetRef, new_commands, target)
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
      Option.map
          (fn rcmds => map Board.anychar (rev rcmds))
          (helper (state, setRef, [], tgt))
    end

  fun string_to_legalchars s = map Board.legalize (explode s)

  fun power_helper (state, visitedSetRef, lchrs, target, power) =
    let
      val moves = map Board.anychar (choice_order_for state target)
      fun move_helper lchrs power (move : Board.legalchar) =
        let
          val sym = Board.piece_symmetry state
          fun body (Board.M {scored, lines, locked, new_phrases = _, status}) =
            (case (locked, status) of
                 (NONE, Board.CONTINUE) =>
                 let val new_lchrs = move::lchrs
                     val pl = piece_location(state, sym, map Board.charcommand new_lchrs)
                     val {px=tx,py=ty,a=ta} = target
                     val ((px, py), pa) = (Board.piece_position state, Board.piece_angle state)
                     val PS {stream_state, ...} = power

(*
                     val _ = print("successfulish move!\n" ^
                                   " piece at " ^ (Int.toString px) ^ " " ^ (Int.toString py) ^ "\n")
                     val _ = print(" lchrs " ^ (lchrs_to_string (rev new_lchrs)) ^ "\n")
*)
                 in
                   if py > ty
                   then ((* print "But rejecting because too low\n";*) NONE)
                   else if (pa, px, py) = (ta, tx, ty)
                   then SOME (new_lchrs, stream_state)
                   else if LocSet.member (!visitedSetRef, pl)
                   then ((*print "But rejecting because already seen\n";*) NONE) (* already visited *)
                   else
                     let
                     in
                       visitedSetRef := (LocSet.add (!visitedSetRef, pl));
                       power_helper (state, visitedSetRef, new_lchrs, target, power)
                     end
                 end
               | _ => NONE
            )
        in
          Board.move_unwind (state, move, body)
        end
      val PS {stream_state, query} = power
      val (phrase, succ, fail) = query stream_state
      val power_word_lchrs = string_to_legalchars phrase
      fun attempt_k true = (* succeeded at saying power word! *)
        ((*print "attempt_k true\n";*)
        power_helper(state,
                     visitedSetRef,
                     (rev power_word_lchrs) @ lchrs,
                     target, PS {stream_state=succ,query=query}))
        | attempt_k false = NONE (* failed at saying power word :( *)
    in
      case Board.move_unwind_many (state, power_word_lchrs, attempt_k) of
          NONE => find_first (move_helper lchrs (PS {stream_state=fail,query=query})) moves
        | x => x
    end

  fun find_with_power state (Target tgt) power =
    let val setRef = ref (LocSet.singleton
                              (piece_location (state, Board.piece_symmetry state, [])));
    in
      Option.map
          (fn (rev_lchrs, stream_state) => (rev rev_lchrs, stream_state))
          (power_helper (state, setRef, [], tgt, power))

    end


  structure PowerHeuristics = struct

    structure StringSet = SplaySetFn(struct
                                      type ord_key = string
                                      val compare = String.compare
                                      end)

    (* Try really hard to get every power word once. Fails spectacularly on problem 1. *)
    datatype basic_state = SSA of StringSet.set
                         | SSB
    fun basic power_phrases =
      let
        fun query (SSA s) = if StringSet.isEmpty s
                            then query SSB
                            else
                              let val pick = hd (StringSet.listItems s)
                              in (pick, SSA (StringSet.delete(s, pick)), SSA s)
                              end
          | query SSB = ("ia! ia!", SSB, SSB)
        val stream_state = SSA (StringSet.addList(StringSet.empty, power_phrases))
      in
        PS {query=query, stream_state=stream_state}
      end

    (* Try really hard to get every power word once, but be less stubborn about ordering *)
    datatype robin_state = RSA of StringSet.set * int
                         | RSB
    fun robin endgame power_phrases =
      let
        fun query (RSA (s, n)) =
          if StringSet.isEmpty s
          then query RSB
          else
            let
              val words_left = StringSet.listItems s
              val pick = List.nth (words_left, n)
            in (pick,
                RSA (StringSet.delete(s, pick), 0),
                RSA (s, (n+1) mod (length words_left)))
            end
          | query RSB = (endgame, RSB, RSB)
        val stream_state = RSA (StringSet.addList(StringSet.empty, power_phrases), 0)
      in
        PS {query=query, stream_state=stream_state}
      end

  end

end
