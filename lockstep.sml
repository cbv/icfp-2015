structure LockStep :> LOCK_STEP = struct

  val HEURISTIC_FACTOR = 1000

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

   (* The state of the board and the position of the last locked piece. *)
   datatype HeuristicInput = HI of {state: Board.state, px: int, py: int, a: int}

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


  datatype searchcontext = SC of { max_depth: int,
                                   heuristic: HeuristicInput -> int,
                                   branch_factor: int,
                                   best: ((int * step list) option) ref,
                                   prev_steps: step list
                                 }


  fun compare_first_reverse ((s1, _), (s2, _)) =
    case Int.compare (s1, s2) of
        LESS => GREATER
      | GREATER => LESS
      | EQUAL => EQUAL
  fun take n [] = []
    | take 0 lst = []
    | take n (x::xs) = x::(take (n - 1) xs)

  (*
     walk through all lockstep sequences of depth n.
     best: ((int, step list) option) ref

     heuristic: Board.state -> int
  *)
  fun search (SC {max_depth, best, branch_factor, heuristic, prev_steps}, combined_score,
              step as Step {state = state_opt, ...}) =
    case (state_opt, max_depth <= 1 + (List.length prev_steps))
     of (SOME(state), false) =>
        let
            val new_context = SC {max_depth = max_depth, branch_factor = branch_factor,
                                  best = best, heuristic = heuristic,
                                  prev_steps = step::prev_steps}
        in
            search_steps (new_context, state)
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

  and search_steps (context as SC {heuristic, prev_steps, branch_factor, ...}, state) =
      let
          fun mapper (step as Step {state = state_opt, px, py, a, ...}) =
            let
                val hscore = case state_opt of
                                 NONE => 0
                               | SOME(state) => heuristic (HI {state = state, px = px, py = py, a = a })
                val scored = List.foldr (fn (Step {scored,...}, s) => scored + s) 0 (step::prev_steps)
                val combined_score = scored * HEURISTIC_FACTOR + hscore
            in
                (combined_score, step)
            end

          val poss = (possible_next_steps state)
          val pairs = List.map mapper poss
          val sorted_pairs = ListUtil.sort compare_first_reverse pairs

          (* Just look at the most promising *)
          val pairs_to_search = take branch_factor sorted_pairs
          fun apper (combined_score, step) =
            let
            in
                search (context, combined_score, step)
            end
      in
          List.app apper pairs_to_search
      end

  fun get_best_step (state, depth, heuristic, step_deadline) =
    let
        val start_time = Time.now()
        val branch_factor = 8;
        val best = ref NONE
        val context = SC {max_depth = depth, heuristic = heuristic, branch_factor = branch_factor,
                          best = best, prev_steps = []}
        val () = search_steps (context, state)
        val end_time = Time.now()
        val time_remaining = Time.-(step_deadline, end_time)
        val elapsed = Time.-(end_time, start_time)

        val enough_time =
            Time.>(time_remaining, Time.fromReal((Time.toReal elapsed) * (Real.fromInt branch_factor)))
    in
        (* we'll deepen if we have enough time and if the best step isn't an end state *)

        case (enough_time, !best) of
            (true, SOME((score, (Step { state = SOME(new_state), ...})::steps)))
             => get_best_step(state, depth + 1, heuristic, step_deadline)
         |  (_, SOME((_, step::steps))) => step
         |  _ => raise LockStep "impossible"
    end

  fun accumulate_best (state, heuristic, accumulator, 0, deadline) = accumulator
    | accumulate_best (state, heuristic, accumulator, steps_remaining, deadline) =
    let
(*        val () = TextIO.output(TextIO.stdErr, Board.toascii state ^ "\n\n\n"); *)
        val step_deadline = Time.+(
                              Time.now(),
                              Time.fromReal(Time.toReal(Time.-(deadline, Time.now())) /
                                            (Real.fromInt steps_remaining)))
        val best_step = get_best_step (state, 1, heuristic, step_deadline)
    in
        case best_step of
            (step as Step {state = SOME(new_state), ...}) =>
            accumulate_best (new_state, heuristic, step::accumulator, steps_remaining - 1, deadline)
         | (step as Step {state = NONE, ...}) => step::accumulator
    end

  fun play_n_steps (state, heuristic, time_limit, max_steps) =
    let
        val deadline = Time.+(time_limit, Time.now())
    in
        accumulate_best (state, heuristic, [], max_steps, deadline)
    end

  fun play_to_end (state, heuristic, time_limit) =
    let
    in
        play_n_steps (state, heuristic, time_limit, (Board.piecesleft state) + 1)
    end

  fun simple_heuristic problem (HI {state, py, ...})  =
    let
        val (width, height) = Board.size problem
        val future_pieces = (Board.piecesleft state) * 50 (* over-estimate *)
        val score = ref future_pieces
        val () = Util.for
                     0 (width - 1)
                     (fn ii => Util.for 0 (height - 1)
                                        (fn jj =>
                                            if Board.isempty (state, ii, jj)
                                            then
                                                let
                                                in
                                                    (* more points, proportional to distance from botton *)
                                                    score := ((!score) + (height - jj) )
                                                end
                                            else ()
                                      ))
    in
        !score
    end


end
