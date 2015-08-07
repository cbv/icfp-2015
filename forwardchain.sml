structure ForwardChain :> FORWARD_CHAIN =
struct
  datatype PieceLocation = PL of {px: int, py: int, a: int, locked: bool}

  fun toascii (PL {px, py, a, locked}) =
    "{(" ^ (Int.toString px) ^ ", " ^ (Int.toString py) ^ ") a:" ^ (Int.toString a) ^ ", locked: "
    ^ Bool.toString(locked) ^ "}"

  fun piece_location (state, locked) =
    let val (px, py) = Board.piece_position state;
         val angle = Board.piece_angle state;
    in
        PL {px = px, py = py, a = angle, locked = locked}
    end

  fun compare (PL {px = px0, py = py0, a = a0, locked = locked0},
               PL {px = px1, py = py1, a = a1, locked = locked1}) =
     if px0 < px1
     then LESS
     else if px0 > px1
     then GREATER
     else if py0 < py1
     then LESS
     else if py0 > py1
     then GREATER
     else if a0 < a1
     then LESS
     else if a0 > a1
     then GREATER
     else if (locked0 andalso not locked1)
     then LESS
     else if (not locked0 andalso locked1)
     then GREATER
     else EQUAL

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
                  else let val () = visitedSetRef := (LocSet.add (!visitedSetRef, pl));
                       in
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
