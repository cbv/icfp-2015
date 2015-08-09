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
        val next_heap = ref (Heap.empty ())
        val _ = Heap.insert (!heap) 0 []
        val iter = ref 0

        fun single_step () =
          case Heap.min (!heap) of
              NONE =>
              if Heap.size (!next_heap) > 0
              then
                  let
                      val () = heap := (!next_heap)
                      val () = next_heap := (Heap.empty())
                      val () =
                          if Heap.size (!heap) > 100000
                          then
                              let
                                  val old_heap = !heap
                                  val new_heap = Heap.empty ()
                                  val () = Util.for 0 100 (fn _ =>
                                                             case Heap.min old_heap
                                                              of SOME(p, v) => (Heap.insert new_heap p v;())
                                                               | _ => ())
                              in
                                  heap := new_heap
                              end
                          else ()
                  in
                      single_step()
                  end
              else ()
            | SOME (neg_combined_score, ssteps) =>
              let
                  val () = iter := ((!iter) + 1)
(*                  val () = print ("examining node with priority " ^ Int.toString neg_combined_score ^ "\n") *)
                  val (state, accum_score) =
                      case ssteps of
                          [] => (initial_state, 0)
                        | (SS { step = Step {state = SOME(state), ...}, accum_score })::_ =>
                          (state, accum_score)
                        | _ => raise LockStep "impossible"
                  val poss = (possible_next_steps state)
                  val pairs_to_search = List.map (compute_combined_score heuristic accum_score) poss

                  fun apper (combined_score, step as Step {scored, state = state_opt, ...}) =
                    let
                        val new_accum_score = accum_score + scored
                        val new_sequence = (SS {step = step, accum_score = new_accum_score})::ssteps
                        val _ =
                            case state_opt of
                                SOME(new_state) =>
                                Heap.insert (!next_heap) (~combined_score) new_sequence
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
                  (if (!iter) mod 1000 = 0 andalso (Heap.size result_heap) > 0
                  then case Heap.min result_heap of
                           SOME (p, v as (SS {accum_score, ...})::_) =>
                           (
                             Heap.insert result_heap p v;
                             print ("found a result with score " ^ Int.toString accum_score ^ "\n");
                             print ("length = " ^ Int.toString (List.length v) ^ "\n")
                           )
                        | _ => raise LockStep "impossible"
                  else ());

                  (if (!iter) mod 1000 = 0
                  then case Heap.min (!heap) of
                           SOME (p, v as (SS {accum_score, step = Step {state= SOME(state), ...}})::_) =>
                           (
                             Heap.insert (!heap) p v;
                             print ("working on something with score " ^ Int.toString accum_score ^ "\n");
                             print ("length = " ^ Int.toString (List.length v) ^ "\n");
                             print (Board.toascii state ^ "\n\n\n")
                           )
                        | _ => raise LockStep "impossible"
                  else ());

                  if (!iter) mod 1000 = 0 andalso Time.>(Time.now(), deadline)
                  then ()
                  else single_step()
              end

        val () = single_step ()
    in
        result_heap
    end


  fun play_to_end (state, heuristic, time_limit) =
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

  fun simple_heuristic problem (HI {state, py, ...})  =
    let
        val (width, height) = Board.size problem
        val alive_bonus = 100000
        val future_pieces = (Board.piecesleft state) * HEURISTIC_FACTOR div 100
    in
        (py * 500) + future_pieces + alive_bonus
    end


end
