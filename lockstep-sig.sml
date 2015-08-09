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

   (* The state of the board and the position of the last locked piece. *)
   datatype HeuristicInput = HI of {state: Board.state, px: int, py: int, a: int}

   (* Takes an initial state, an heuristic, and a time limit.
      The heuristic is used in evaluation of leaf nodes and in
      prioritizing which branches to explore. In the full evaluation function,
      real actual points are worth 10000 times as much as heuristic points.
      Returns the 'best' sequence of steps to take, in reverse order. *)
   val play_to_end : Board.state * (HeuristicInput -> int) * Time.time -> step list

   (* Like play_to_end, but returns at most n steps. *)
   val play_n_steps : Board.state * (HeuristicInput -> int) * Time.time * int -> step list


  (* points for open cells with small y coordinate *)
   val simple_heuristic : Board.problem -> HeuristicInput -> int
end
