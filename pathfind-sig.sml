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

  val find_with_power :
    Board.state -> target -> 'a power_stream ->
    (* This a reversed command list that implements it, but without power *)
    Board.command list ->
    (Board.legalchar list * 'a) option

  (* Same as above, but don't try to insert power words -- just try
     to be done immediately (used when we run out of time budget). *)
  val find_without_power :
    Board.state -> target -> 'a power_stream ->
    Board.command list ->
    (Board.legalchar list * 'a) option

  structure PowerHeuristics :
  sig
    type basic_state
    val basic : string list -> basic_state power_stream

    type robin_state
    val robin : string -> string list -> robin_state power_stream
  end
end
