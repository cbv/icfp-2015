signature LOCK_STEP = sig

  exception LockStep of string

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
                     commands: Board.command list,
                     (* An example number of points scored for making this step. *)
                     scored: int
                 }


   val possible_next_steps: Board.state -> step list

   (* Takes an initial state and an heuristic. Returns the 'best' sequence of steps to take. *)
   val play_to_end : Board.state * (Board.state -> int) -> step list

end
