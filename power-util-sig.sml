signature POWER_UTIL =
sig

  val loadproblem : int -> Board.problem
  val problems : unit -> Board.problem vector

  val get_valid_prefix : Board.problem -> Word32.word -> string -> int

  (* Given all the problems to try, and a solution, return the
     (prefix size, problem index, seed) for which the prefix can be
     longest while still being valid. *)
  val longest_prefix : Board.problem vector -> string ->
    int * int * Word32.word

end