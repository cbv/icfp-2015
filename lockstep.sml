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

  structure Heap = HeapFn(struct
                           type priority = int
                           val compare = Int.compare
                           end)


  fun possible_next_steps_n n heuristic accum state =
    let
        val heap = Heap.empty()
        val accessible = ForwardChain.accessible_locations state
        fun apper (ForwardChain.PL {locked = NONE, ... }) = ()
          | apper (ForwardChain.PL {locked = (SOME (ForwardChain.NEW_PIECE state)),
                                     px, py, a, commands, score, ... }) =
            (Heap.insert heap (~((score + accum) * HEURISTIC_FACTOR + (heuristic (HI {state = state,
                                                                           px = px, py = py, a = a}))))
                        (Step {px = px, py = py, a = a, commands = commands,
                               state = SOME(Board.clone state), scored = score});())
          | apper (ForwardChain.PL {locked = (SOME ForwardChain.ALL_DONE),
                                     px, py, a, commands, score, ... }) =
            (Heap.insert heap (~((score + accum)* HEURISTIC_FACTOR))
                         (Step {px = px, py = py, a = a, commands = commands,
                                state = NONE, scored = score}); ())
        val () = List.app apper accessible
        fun helper acc 0 = acc
          | helper acc m =
            case Heap.min heap of
                NONE => acc
              | SOME (p,v) => helper ((p,v)::acc) (m-1)
        val result = helper [] n
    in
        result
    end


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
        val best_result = ref NONE
        fun update_best_result (accum_score, ssteps) =
          case !best_result of
              NONE => (best_result := SOME(accum_score, ssteps))
           |  SOME(best_score, best_steps) =>
              if accum_score > best_score
              then best_result := (SOME(accum_score, ssteps))
              else ()
        val heap = ref (Heap.empty ())
        val next_heap = ref (Heap.empty ())
        val _ = Heap.insert (!heap) 0 []
        val iter = ref 0
        val step_start_time = ref (Time.now())
        val step_node_count = ref 0
        val pieces_left = ref (Board.piecesleft initial_state)

        fun search_loop () =
          case Heap.min (!heap) of
              NONE =>
              if Heap.size (!next_heap) > 0
              then
                  let
(*                       val () = print ("stepping. next heap size = " ^
                                      Int.toString (Heap.size (!next_heap)) ^ "\n") *)
                      val () = (pieces_left := ((!pieces_left) - 1))
                      val num_nodes_last_step = !step_node_count
                      val () = (step_node_count := 0)
                      val now = Time.now()
                      val elapsed = Time.-(now, !step_start_time)
                      val time_per_node = (Time.toReal elapsed) / (Real.fromInt num_nodes_last_step)
                      val time_left = Time.-(deadline, now)
                      val time_per_remaining_step =
                          if (!pieces_left) > 0
                          then (Time.toReal time_left) / (Real.fromInt ((!pieces_left) + 1 ))
                                                                    (* plus one to be safe *)
                          else 1.0 (* prevent overflow *)
                      val nodes_per_step' = Int.max(1, Real.floor (time_per_remaining_step / time_per_node))
                      val nodes_per_step = Int.min(10000, nodes_per_step')
(*                      val () = print ("nodes per step: " ^ Int.toString nodes_per_step ^ "\n") *)
                      val () = (step_start_time := now)
                      val () = heap := (!next_heap)
                      val () = next_heap := (Heap.empty())
                      val () =
                          if Heap.size (!heap) > nodes_per_step
                          then
                              let
                                  val old_heap = !heap
                                  val new_heap = Heap.empty ()
                                  val () = Util.for 0 (nodes_per_step-1) (fn _ =>
                                                             case Heap.min old_heap
                                                              of SOME(p, v) => (
                                                                  Heap.insert new_heap p v;
                                                                  ())
                                                               | _ => ())
                              in
                                  heap := new_heap
                              end
                          else ()
                  in
                      if Time.>(time_left, Time.zeroTime)
                      then search_loop()
                      else
                          let (* make sure the result queue has at least something in it. *)
                              val _ = case Heap.min (!heap) of
                                          SOME (p, ssteps as (ss as SS {step, accum_score})::_) =>
                                          update_best_result (accum_score, ssteps)
                                       | _ => raise LockStep "but I justed inserted!?"
                          in
                              ()
                          end

                  end
              else ()
            | SOME (neg_combined_score, ssteps) =>
              let
                  val () = iter := ((!iter) + 1)
                  val () = step_node_count := ((!step_node_count) + 1)
(*                  val () = print ("examining node with priority " ^ Int.toString neg_combined_score ^ "\n") *)
                  val (state, accum_score) =
                      case ssteps of
                          [] => (initial_state, 0)
                        | (SS { step = Step {state = SOME(state), ...}, accum_score })::_ =>
                          (state, accum_score)
                        | _ => raise LockStep "impossible 1"

                  val branching_factor = 8

                  val pairs_to_search = possible_next_steps_n branching_factor heuristic accum_score state

                  fun apper (combined_score, step as Step {scored, state = state_opt, ...}) =
                    let
                        val new_accum_score = accum_score + scored
                        val new_sequence = (SS {step = step, accum_score = new_accum_score})::ssteps
                        val _ =
                            case state_opt of
                                SOME(new_state) =>
                                (Heap.insert (!next_heap) (combined_score) new_sequence;())
                              | NONE =>
                                (* We've reached an end state. emit it. *)
                                update_best_result (new_accum_score, new_sequence)
                    in
                        ()
                    end
              in
                  List.app apper pairs_to_search;
(*
                  print ("took a step. size = " ^ Int.toString (Heap.size (!heap)) ^ "\n");
                  print ("result size = " ^ Int.toString (Heap.size result_heap) ^ "\n"); *)

(*
                  (if (!iter) mod 1000 = 0
                  then case Heap.min (!heap) of
                           SOME (p, v as (SS {accum_score, step = Step {state= SOME(state), ...}})::_) =>
                           (
                             Heap.insert (!heap) p v;
                             print ("working on something with score " ^ Int.toString accum_score ^ "\n");
                             print ("and priority " ^ Int.toString p ^ "\n");
                             print ("length = " ^ Int.toString (List.length v) ^ "\n");
                             print (Board.toascii state ^ "\n\n\n")
                           )
                        | _ => ()
                  else ());
*)

                  search_loop ()
              end

        val () = search_loop ()
    in
        case !best_result of
            SOME(s, ssteps) => ssteps
         |  NONE => raise LockStep "impossible?"
    end


  fun play_to_end (state, heuristic, time_limit) =
    let
        val deadline = Time.+(time_limit, Time.now())
        val scored_steps = accumulate_best (state, heuristic, deadline)
        val steps = List.map (fn SS {step, ...} => step) scored_steps
    in
        steps
    end

  fun simple_heuristic problem (HI {state, py, ...})  =
    let
        val (width, height) = Board.size problem
        val alive_bonus = 100000
        val future_pieces = (Board.piecesleft state) * HEURISTIC_FACTOR div 100
    in
        (py * 500) + future_pieces + alive_bonus
    end


end
