structure Powerize :> POWERIZE =
struct

  fun insert state tgt phop =
    let val phop_lchars = map Board.legalize (explode phop)
    in
      Board.move_unwind_many (state,
                        phop_lchars,
                        fn valid =>
                           if valid
                           then Option.map (fn result => phop_lchars @ result)
                                           (Pathfind.find state tgt)
                           else NONE)
    end


end
