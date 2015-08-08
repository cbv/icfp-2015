structure LockStep :> LOCK_STEP = struct

   (* Lock the piece at position `(px, py)` and angle `a` *)
   datatype step = Step of {
                     px: int,
                     py: int,
                     a: int,

                     (* An example state that could result from taking this step. *)
                     state: Board.state,
                     (* An example list of commands that could make up this step. *)
                     commands: Board.command list
                 }

  fun possible_next_steps (Step {state, ...}) =
    let
        val accessible = ForwardChain.accessible_locations state
        fun mapper (ForwardChain.PL {locked = NONE, ... }) = NONE
          | mapper (ForwardChain.PL {locked = (SOME state), px, py, a, commands, ... }) =
            SOME (Step {px = px, py = py, a = a, commands = commands, state = state })
    in
        List.mapPartial mapper accessible
    end

end
