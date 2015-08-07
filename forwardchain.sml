structure ForwardChain :> FORWARD_CHAIN =
struct
  datatype PieceLocation = PL of {px: int, py: int, a: int, locked: bool}

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

  val moves = [Board.anychar (Board.D Board.E), Board.anychar (Board.D Board.W),
               Board.anychar (Board.D Board.SE), Board.anychar (Board.D Board.SW),
               Board.anychar (Board.T Board.CW), Board.anychar (Board.T Board.CCW)
              ]


  fun move_helper state move =
    let val {result, undo} = Board.move_undo (state);
        val () =
        (case result of
             Board.Continue {scored, lines, locked} =>
              let (* val pl = piece_location(state, locked) *)
              in ()
              end
          |  Board.Done {reason}  => ())
    in
        undo ();
        ()
    end

  fun helper (state, visitedSet) =
    List.app (move_helper state) moves

  fun accessible_locations state =
    let val set = LocSet.singleton (piece_location (state, false)); (* XXX locked? *)
    in
        LocSet.listItems set
    end

end
