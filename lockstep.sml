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
                     (* An example list of commands that could make up this step, in reverse order *)
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
     walk through all lockstep sequences of depth n.
     best: ((int, step list) option) ref

     heuristic: Board.state -> int
  *)
  fun search (max_depth, best, heuristic, heuristic_score, prev_steps) (step as Step {scored, state = state_opt, ...}) =
    case (state_opt, max_depth <= 1 + (List.length prev_steps))
     of (SOME(state), false) =>
        let
        in
            search_steps (max_depth, best, heuristic, state, step::prev_steps)
        end
     | _ => (* don't go deeper *)
       let
           val best_score = case !best of
                                SOME((score, _)) => score
                              | NONE => ~1
           val combined_score = 10000 * heuristic_score
           val () = if combined_score > best_score
                    then best := (SOME((combined_score, step::prev_steps)))
                    else ()
       in
           ()
       end

  and search_steps (max_depth, best, heuristic, state, prev_steps) =
      let
          fun apper (step as Step {state, ...}) =
            let
                val hscore = case state of
                                 NONE => 0
                               | SOME(state) => heuristic state
            in
                (search (max_depth, best, heuristic, hscore, prev_steps) step)
            end
      in
          List.app apper (possible_next_steps state)
      end

  fun accumulate_best (state, heuristic, accumulator) =
    let
        val best = ref NONE
        val () = search_steps (3, best, heuristic, state, [])
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
