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
                     (* An example list of commands that could make up this step, in reverse order. *)
                     commands: Board.command list,
                     (* An example number of points scored for making this step. *)
                     scored: int
                 }

   val stepstring: step -> string

   val possible_next_steps: Board.state -> step list

   val HEURISTIC_FACTOR : int (* = 1000 *)

   (* The state of the board and the position of the last locked piece.
      A heuristic function is used in evaluation of leaf nodes and in
      prioritizing which branches to explore.

     You should aim to satisfy:

     (value of heuristic) / HEURISTIC_FACTOR = the expected number of real actual points
                                               winnable from this state in the future.
    *)
   datatype HeuristicInput = HI of {state: Board.state, px: int, py: int, a: int}

   (* Takes an initial state, an heuristic, and a time limit.
      Returns the 'best' sequence of steps to take, in reverse order. *)
   val play_to_end : Board.state * (HeuristicInput -> int) * Time.time -> step list

  (* likes it when the previous lock had a large y coordinate *)
   val simple_heuristic : Board.problem -> HeuristicInput -> int

end
