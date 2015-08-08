signature LOCK_STEP = sig

   (* Lock the piece at position `(px, py)` and angle `a` *)
   datatype step = Step of {
                     px: int,
                     py: int,
                     a: int,

                     (* An example state that could result from taking this step.
                       NONE if this step results in gameover. You can assume that this state
                       is not aliased. *)
                     state: Board.state option,
                     (* An example list of commands that could make up this step. *)
                     commands: Board.command list
                     (* *)
(*                     scored: int  *)
                 }


   val possible_next_steps: Board.state -> step list

end
