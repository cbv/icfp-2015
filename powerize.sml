structure Powerize :> POWERIZE =
struct

  (* we throw away information about score, lines, lock position and just
     keep whether we can freely make all these moves *)
  fun move_unwind_many (s, [], k) = k true
    | move_unwind_many (s, c::cs, k) =
      Board.move_unwind (s, c,
                   fn (Board.M {locked, status, ...}) =>
                      (case (locked, status) of
                           (NONE, Board.CONTINUE) => move_unwind_many (s, cs, k)
                         | _ => k false))

  fun insert state tgt phop =
    let val phop_lchars = map Board.legalize (explode phop)
    in
      move_unwind_many (state,
                        phop_lchars,
                        fn valid =>
                           if valid
                           then Option.map (fn result => phop_lchars @ result)
                                           (Pathfind.find state tgt)
                           else NONE)
    end

end
