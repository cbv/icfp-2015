signature LOCK_STEP = sig

   (* Lock the piece at position `(px, py)` and angle `a` *)
   datatype step = Step of {
                     px: int,
                     py: int,
                     a: int,

                     (* An example state that could result from taking this step. *)
                     state: Board.state,
                     (* An example list of commands that could make up this step. *)
                     commands: Board.command list
                     (* *)
(*                     scored: int  *)
                 }


   val possible_next_steps : step -> step list

end
