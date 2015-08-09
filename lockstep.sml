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



  structure Heap = HeapFn(struct
                           type priority = int
                           val compare = Int.compare
                           end)

  datatype ScoredStep = SS of {
         step: step,

         (* Total points achieved up in all of history up to here.
            This does not include an *)
         accum_score: int
  }

  fun compute_combined_score heuristic accum_score
                             (step as Step {state = state_opt, px, py, a, scored, ...}) =
    let
        val hscore = case state_opt of
                         NONE => 0
                       | SOME(state) => heuristic (HI {state = state, px = px, py = py, a = a })
        val combined_score = (accum_score + scored) * HEURISTIC_FACTOR + hscore
    in
        (combined_score, step)
    end

  fun accumulate_best (initial_state, heuristic, deadline) =
    let
        val result_heap = Heap.empty ()
        val heap = ref (Heap.empty ())
        val _ = Heap.insert (!heap) 0 []
        val iter = ref 0

        fun maybe_prune() =
          if Heap.size (!heap) < 100000
          then ()
          else
              let
                  val old_heap = !heap
                  val new_heap = Heap.empty()
                  val () = Util.for 0 1000 (fn _ =>
                                               case Heap.min old_heap
                                                of SOME(p, v) => (Heap.insert new_heap p v;())
                                                 | _ => ())
              in
                  heap := new_heap
              end

        fun single_step () =
          case Heap.min (!heap) of
              NONE => ()
            | SOME (neg_combined_score, ssteps) =>
              let
                  val () = maybe_prune()
(*                  val () = print ("examining node with priority " ^ Int.toString neg_combined_score ^ "\n") *)
                  val (state, accum_score) =
                      case ssteps of
                          [] => (initial_state, 0)
                        | (SS { step = Step {state = SOME(state), ...}, accum_score })::_ =>
                          (state, accum_score)
                        | _ => raise LockStep "impossible"
                  val poss = (possible_next_steps state)
                  val pairs = List.map (compute_combined_score heuristic accum_score) poss
                  val sorted_pairs = ListUtil.sort compare_first_reverse pairs

                  (* Just look at the most promising *)
                  val branch_factor = 8
                  val pairs_to_search = take branch_factor sorted_pairs
                  fun apper (combined_score, step as Step {scored, state = state_opt, ...}) =
                    let
                        val new_accum_score = accum_score + scored
                        val new_sequence = (SS {step = step, accum_score = new_accum_score})::ssteps
                        val _ =
                            case state_opt of
                                SOME(new_state) =>
                                Heap.insert (!heap) (~combined_score) new_sequence
                              | NONE =>
                                (* We've reached an end state. emit it. *)
                                Heap.insert result_heap (~new_accum_score)
                                            new_sequence
                    in
                        ()
                    end
              in
                  List.app apper pairs_to_search;
(*                  print ("took a step. size = " ^ Int.toString (Heap.size (!heap)) ^ "\n");
                  print ("result size = " ^ Int.toString (Heap.size result_heap) ^ "\n"); *)
                  single_step()
              end

        val () = single_step ()
    in
        result_heap
    end

  fun play_n_steps (state, heuristic, time_limit, max_steps) =
    let
        val deadline = Time.+(time_limit, Time.now())
        val result_heap = accumulate_best (state, heuristic, deadline)
        val scored_steps =
            case Heap.min result_heap of
                SOME(_, ssteps) => ssteps
              | NONE => raise LockStep "impossible?"
        val steps = List.map (fn SS {step, ...} => step) scored_steps
    in
        steps
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
