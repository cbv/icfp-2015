structure ForwardChain :> FORWARD_CHAIN =
struct

  exception ForwardChain of string

  datatype LockedState = NEW_PIECE of Board.state | ALL_DONE
  datatype PieceLocation = PL of {px: int, py: int, a: int,
                                  locked: LockedState option,
                                  score: int,
                                  commands: Board.command list (* TODO(perf) track this somewhere else? *)}

  fun toascii (PL {px, py, a, locked, ...}) =
    "{(" ^ (Int.toString px) ^ ", " ^ (Int.toString py) ^ ") a:" ^ (Int.toString a) ^ ", locked: "
    ^ Bool.toString(Option.isSome locked) ^ "}"

  fun piece_location (state, sym, locked, status, score, commands) =
    let
      val ((px, py), angle, locked_state) =
        case (locked, status) of
            (SOME (x,y,a), Board.CONTINUE) => ((x,y), a, SOME(NEW_PIECE(Board.clone state)))
          | (SOME (x,y,a), (Board.GAMEOVER _)) => ((x,y), a, SOME(ALL_DONE))
          | (NONE, _) => (Board.piece_position state, Board.piece_angle state, NONE)
          | _ => raise ForwardChain "impossible"
    in
      PL {px = px, py = py, a = angle mod sym, locked = locked_state,
          score = score, commands = commands}
    end

  fun compare (PL {px = px0, py = py0, a = a0, locked = locked0, ...},
               PL {px = px1, py = py1, a = a1, locked = locked1, ...}) =
    case Int.compare (px0, px1) of
      EQUAL => (case Int.compare (py0, py1) of
                  EQUAL => (case Int.compare (a0, a1) of
                              EQUAL => (case (locked0, locked1) of
                                            (SOME(_), NONE) => GREATER
                                         | (NONE, SOME(_)) => LESS
                                         | _ => EQUAL
                                       )
                            | other => other)
                | other => other)
    | other => other

  structure LocSet = SplaySetFn(struct
                                   type ord_key = PieceLocation
                                   val compare = compare
                                end)

  datatype command = datatype Board.command
  datatype dir = datatype Board.dir
  datatype turn = datatype Board.turn
  val moves = map Board.anychar [(D SE), (D SW),
                                 (D E),  (D W),
                                 (T CW), (T CCW)]

  fun move_helper (state, visitedSetRef, score, commands) move =
    let
        val sym = Board.piece_symmetry state
        fun body (Board.M {scored, lines, locked, status}) =
          (case status of
              Board.ERROR => ()
            |  _ =>
               let val new_commands = (Board.charcommand move)::commands
                   val new_score = score + scored
                   val pl = piece_location(state, sym, locked, status, new_score, new_commands)
               in
                   if LocSet.member (!visitedSetRef, pl)
                   then () (* already visited *)
                   else
                       let
                       in
                           visitedSetRef := (LocSet.add (!visitedSetRef, pl));
                           case locked of
                               NONE => helper (state, visitedSetRef, new_score, new_commands)
                            |  SOME _ =>  ()
                       end
               end
          )
    in
        Board.move_unwind (state, move, body)
    end

  and helper (state, visitedSetRef, score, commands) =
    List.app (move_helper (state, visitedSetRef, score, commands)) moves

  fun accessible_locations state =
    let val setRef = ref (LocSet.singleton
                              (piece_location
                                   (state, Board.piece_symmetry state,
                                    NONE, Board.CONTINUE, 0, []))); (* can't be locked on first turn *)
        val () = helper (state, setRef, 0, []);
    in
        LocSet.listItems (!setRef)
    end

  fun combine_score (score, heur_score) = 10000 * score + heur_score

  (* Board.state -> (list of commands (reversed), bool indicating whether to continue) *)
  fun simple_heuristic_step (state, heuristic) =
    let
        val locs = accessible_locations state
        val best_score = ref (~1)
        val best_loc = ref NONE
        fun do_loc (PL {locked = NONE, ...}) = ()
          | do_loc (loc as PL {score, px, py, locked = _, ...}) =
            let
                val combined_score = combine_score(score, heuristic (px, py))
            in
                if combined_score > (!best_score)
                then ((best_score := combined_score);
                      (best_loc := (SOME loc)))
                else ()
            end
        val () = List.app do_loc locs
    in
        case !best_loc of
            NONE => ([], false)
          | SOME (PL {commands, locked = SOME(ALL_DONE), ...}) => (commands, false)
          | SOME (PL {commands, locked = _ , ...}) => (commands, true)
    end

  fun stepper (state, heuristic, accumulator) =
    let
        val (rev_commands, continue) = simple_heuristic_step (state, heuristic)
        val () = List.app (fn c =>
                              let
                                 val result =  Board.move (state, Board.anychar c)
                              in
                                  ()
                              end)
                          (List.rev rev_commands)
        val acc' = rev_commands::accumulator
    in
        if continue
        then (stepper (state, heuristic, acc'))
        else List.rev (List.concat acc')
    end

  fun simple_heuristic_solver (state, heuristic) =
    stepper (state, heuristic, [])

end
