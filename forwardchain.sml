structure ForwardChain :> FORWARD_CHAIN =
struct
  datatype PieceLocation = PL of {px: int, py: int, a: int, locked: bool, score: int,
                                  commands: Board.command list (* TODO(perf) track this somewhere else? *)}

  fun toascii (PL {px, py, a, locked, ...}) =
    "{(" ^ (Int.toString px) ^ ", " ^ (Int.toString py) ^ ") a:" ^ (Int.toString a) ^ ", locked: "
    ^ Bool.toString(locked) ^ "}"

  fun piece_location (state, sym, locked, score, commands) =
    let
      val ((px, py), angle, is_locked) = case locked of
                                             SOME (x,y,a) => ((x,y), a, true)
                                           | NONE => (Board.piece_position state, Board.piece_angle state, false)
    in
        PL {px = px, py = py, a = angle mod sym, locked = is_locked, score = score, commands = commands}
    end

  fun compare (PL {px = px0, py = py0, a = a0, locked = locked0, ...},
               PL {px = px1, py = py1, a = a1, locked = locked1, ...}) =
    case Int.compare (px0, px1) of
      EQUAL => (case Int.compare (py0, py1) of
                  EQUAL => (case Int.compare (a0, a1) of
                              EQUAL => Util.bool_compare (locked0, locked1)
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
  val moves = map Board.anychar [(D E), (D W),
                                 (D SE), (D SW),
                                 (T CW), (T CCW)]

  fun move_helper (state, visitedSetRef, commands) move =
    let val {result = Board.M {scored, lines, locked, status}, undo} =
          Board.move_undo (state, move)
        val sym = Board.piece_symmetry state
        val () =
        (case status of
             Board.CONTINUE =>
              let val new_commands = (Board.charcommand move)::commands
                  val pl = piece_location(state, sym, locked, scored, new_commands)
              in
                  if LocSet.member (!visitedSetRef, pl)
                  then () (* already visited *)
                  else let
                       in
                           visitedSetRef := (LocSet.add (!visitedSetRef, pl));
                           case locked of
                               NONE => helper (state, visitedSetRef, new_commands)
                            |  SOME _ =>  ()
                       end
              end
          | Board.COMPLETE => ()
          | Board.NO_SPACE => ()
          | Board.ERROR => ())
    in
        undo ()
    end

  and helper (state, visitedSetRef, commands) =
    List.app (move_helper (state, visitedSetRef, commands)) moves

  fun accessible_locations state =
    let val setRef = ref (LocSet.singleton (piece_location (state, Board.piece_symmetry state,
                                                            NONE, 0, []))); (* can't be locked on first turn *)
        val () = helper (state, setRef, []);
    in
        LocSet.listItems (!setRef)
    end

  (* Board.state -> (list of commands (reversed), bool indicating whether to continue) *)
  fun simple_heuristic_step state =
    let
        val locs = accessible_locations state
        val best_score = ref (~1)
        val best_loc = ref NONE
        val () = List.app (fn (loc as PL {score, ...}) =>
                              if score > (!best_score)
                              then ((best_score := score);
                                    (best_loc := (SOME loc)))
                              else ())
                          locs
    in
        case !best_loc of
            NONE => ([], false)
          | SOME (PL {commands, ...}) => (commands, true)
    end

  fun stepper (state, accumulator) =
    let
        val (rev_commands, continue) = simple_heuristic_step state
        val () = List.app (fn c =>
                              let
                                 val result =  Board.move (state, Board.anychar c)
                                 val () = print ((Board.moveresultstring result) ^ "\n")
                              in
                                  ()
                              end)
                          (List.rev rev_commands)
        val acc' = rev_commands::accumulator
    in
        if continue
        then (stepper (state, acc'))
        else List.rev (List.concat accumulator)
    end

  fun simple_heuristic_solver state =
    stepper (state, [])

end
