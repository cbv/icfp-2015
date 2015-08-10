
structure PowerThirst =
struct

  fun polish seconds initial_state
    (powerstream as Pathfind.PS {stream_state=init_stream_state, query}) steplist =
    let
      fun lchrs_for_step state stream_state (LockStep.Step {px, py, a, commands, ...}) =
        let
          val powa = Pathfind.find_with_power state
            (Pathfind.Target {px=px, py=py, a=a})
            (Pathfind.PS { stream_state = stream_state,
                           query = query })
            commands
          val lock_lchr = Board.anychar (hd commands)
        in
          case powa of
            SOME (x, stream_state') => (x @ [lock_lchr], stream_state')
          | NONE => (* shouldn't happen, but just in case... *)
              (map Board.anychar (rev commands), stream_state)
        end

      fun loopsteps state stream_state [] = raise Match
        | loopsteps state stream_state ((step as LockStep.Step {state=next_state, ...})::tl) =
        let
          val (this, stream_state') = lchrs_for_step state stream_state step
          val rest = (case next_state of
                        SOME state' => loopsteps state' stream_state' tl
                      | NONE => [])
        in
          this @ rest
        end
    in
      loopsteps initial_state init_stream_state steplist
    end

end
