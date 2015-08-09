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

   fun stepstring (Step {px, py, a, commands, scored, ...}) =
     "{ px = " ^ Int.toString px ^ ", py = " ^ Int.toString py ^ ", a = " ^ Int.toString a ^
        ", scored = " ^ Int.toString scored ^
        ", commands= " ^  String.concat (List.map (fn c => (Board.commandstring c ^ ",")) commands)  ^ "}"

  fun possible_next_steps state =
    let
        val accessible = ForwardChain.accessible_locations state
        fun mapper (ForwardChain.PL {locked = NONE, ... }) = NONE
          | mapper (ForwardChain.PL {locked = (SOME (ForwardChain.NEW_PIECE state)),
                                     px, py, a, commands, score, ... }) =
            SOME (Step {px = px, py = py, a = a, commands = commands,
                        state = SOME(Board.clone state), scored = score})
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
  fun search (max_depth, best, heuristic, combined_score, prev_steps) (step as Step {state = state_opt, ...}) =
    case (state_opt, max_depth <= 1 + (List.length prev_steps))
     of (SOME(state), false) =>
        let
        in
            search_steps (max_depth, best, heuristic, state, step::prev_steps)
        end
     | _ => (* don't go deeper *)
       let
           val steps = step::prev_steps
           val best_score = case !best of
                                SOME((score, _)) => score
                              | NONE => ~1
           val () = if combined_score > best_score
                    then best := (SOME((combined_score, List.rev steps)))
                    else ()
       in
           ()
       end

  and search_steps (max_depth, best, heuristic, state, prev_steps) =
      let
          fun mapper (step as Step {state, ...}) =
            let
                val hscore = case state of
                                 NONE => 0
                               | SOME(state) => heuristic state
                val scored = List.foldr (fn (Step {scored,...}, s) => scored + s) 0 (step::prev_steps)
                val combined_score = 10000 * scored + hscore

            in
                (combined_score, step)
            end

                (* reverse order *)
          fun compare ((s1, _), (s2, _)) =
            case Int.compare (s1, s2) of
                LESS => GREATER
              | GREATER => LESS
              | EQUAL => EQUAL
          fun take n [] = []
            | take 0 lst = []
            | take n (x::xs) = x::(take (n - 1) xs)
          val poss = (possible_next_steps state)
          val pairs = List.map mapper poss
          val sorted_pairs = ListUtil.sort compare pairs
          (* Just look at the most promising *)
          val pairs_to_search = take 8 sorted_pairs
          fun apper (combined_score, step) =
            let
            in
                (search (max_depth, best, heuristic, combined_score, prev_steps) step)
            end
      in
          List.app apper pairs_to_search
      end

  fun accumulate_best (state, heuristic, accumulator) =
    let
        val best = ref NONE
        val () = search_steps (6, best, heuristic, state, [])
    in
        case !best of
            SOME((score, all_steps as (step as Step { state = SOME(state), ...})::steps)) =>
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
