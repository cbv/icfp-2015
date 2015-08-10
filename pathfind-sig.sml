signature PATHFIND = sig
  datatype target = Target of {px: int, py: int, a: int}
  val find : Board.state -> target -> Board.legalchar list option

  datatype 'a power_stream = PS of {stream_state: 'a,
                                    (* given a stream-state, return a
                                       power phrase to attempt, a
                                       state to transition to if it
                                       succeeds, and a state to
                                       transition to if it fails. *)
                                       query: 'a -> string * 'a * 'a}

  val find_with_power : Board.state -> target -> 'a power_stream ->
                        (Board.legalchar list * 'a) option

  structure PowerHeuristics :
            sig
              type basic_state
              val basic : string list -> basic_state power_stream

              type robin_state
              val robin : string -> string list -> robin_state power_stream
            end
end
