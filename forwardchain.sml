structure ForwardChain :> FORWARD_CHAIN = struct
  datatype PieceLocation = PL of {px: int, py: int, a: int, fixed: bool}

  fun compare (PL {px = px0, py = py0, a = a0, fixed = fixed0},
               PL {px = px1, py = py1, a = a1, fixed = fixed1}) =
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
     else if (fixed0 andalso not fixed1)
     then LESS
     else if (not fixed0 andalso fixed1)
     then GREATER
     else EQUAL


  structure LocSet = SplaySetFn(struct
                                   type ord_key = PieceLocation
                                   val compare = compare
                                   end)

  fun accessible_locations state =
    let val (px, py) = piece_position state;
        val angle = piece_angle state;
        val set = LocSet.singleton (PL {px = px, py = py, a = angle, fixed = false}); (* XXX fixed? *)
    in
        LocSet.listItems set
    end

end
