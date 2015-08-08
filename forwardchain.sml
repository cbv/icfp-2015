structure ForwardChain :> FORWARD_CHAIN =
struct
  datatype PieceLocation = PL of {px: int, py: int, a: int, locked: bool}

  fun toascii (PL {px, py, a, locked}) =
    "{(" ^ (Int.toString px) ^ ", " ^ (Int.toString py) ^ ") a:" ^ (Int.toString a) ^ ", locked: "
    ^ Bool.toString(locked) ^ "}"

  fun piece_location (state, locked) =
    let
      val (px, py) = Board.piece_position state
      val angle = Board.piece_angle state
      val symmetry = Board.piece_symmetry state
    in
        PL {px = px, py = py, a = angle mod symmetry, locked = locked}
    end

  fun compare (PL {px = px0, py = py0, a = a0, locked = locked0},
               PL {px = px1, py = py1, a = a1, locked = locked1}) =
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

  fun move_helper (state, visitedSetRef) move =
    let val {result = Board.M {scored, lines, locked, status}, undo} =
          Board.move_undo (state, move)
        val () =
        (case status of
             Board.CONTINUE =>
              let val pl = piece_location(state, locked)
              in
                  if LocSet.member (!visitedSetRef, pl)
                  then () (* already visited *)
                  else let
                       in
                           visitedSetRef := (LocSet.add (!visitedSetRef, pl));
                           if not locked
                           then helper (state, visitedSetRef)
                           else ()
                       end
              end
          | Board.COMPLETE => ()
          | Board.NO_SPACE => ()
          | Board.ERROR => ())
    in
        undo ()
    end

  and helper (state, visitedSetRef) =
    List.app (move_helper (state, visitedSetRef)) moves

  fun accessible_locations state =
    let val setRef = ref (LocSet.singleton (piece_location (state, false))); (* XXX locked? *)
        val () = helper (state, setRef);
    in
        LocSet.listItems (!setRef)
    end

end
