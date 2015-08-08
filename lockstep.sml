structure LockStep :> LOCK_STEP = struct

  exception LockStep of string

   (* Lock the piece at position `(px, py)` and angle `a` *)
   datatype step = Step of {
                     px: int,
                     py: int,
                     a: int,

                     (* An example state that could result from taking this step.
                       NONE if this step results in gameover. *)
                     state: Board.state option,
                     (* An example list of commands that could make up this step. *)
                     commands: Board.command list,
                     (* An example number of points scored for making this step. *)
                     scored: int
                 }

  fun possible_next_steps state =
    let
        val accessible = ForwardChain.accessible_locations state
        fun mapper (ForwardChain.PL {locked = NONE, ... }) = NONE
          | mapper (ForwardChain.PL {locked = (SOME (ForwardChain.NEW_PIECE state)),
                                     px, py, a, commands, score, ... }) =
            SOME (Step {px = px, py = py, a = a, commands = commands, state = SOME(state), scored = score})
          | mapper (ForwardChain.PL {locked = (SOME ForwardChain.ALL_DONE),
                                     px, py, a, commands, score, ... }) =
            SOME (Step {px = px, py = py, a = a, commands = commands, state = NONE, scored = score })
    in
        List.mapPartial mapper accessible
    end


  (*
     enumerate all lockstep sequences of depth n.
     best: ((int, step list) option) ref

     heuristic: Board.state -> int
  *)
  fun search (max_depth, best, (step as Step {px, py, a, state = SOME(state), ...}), prev_steps) =
    let
        val _ = possible_next_steps state
    in
        ()
    end
    | search (max_depth, best, (step as Step {px, py, a, state = NONE, ...}), prev_steps) = ()


  fun accumulate_best (state, heuristic, accumulator) =
    let
        val best = ref NONE
(*        val () = search (3, best, [], best) *)
    in
        case !best of
            SOME((score, (step as Step { state = SOME(state), ...})::steps)) =>
            accumulate_best (state, heuristic, step::accumulator)
         |  SOME((score, (step as Step { state = NONE, ...})::steps)) => step::accumulator
         |  _ => raise LockStep "impossible"
    end

  fun play_to_end (state, heuristic) =
    let
    in
        accumulate_best (state, heuristic, [])
    end

end
