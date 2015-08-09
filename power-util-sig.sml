(* Needs excluded.sml (large data file) so probably shouldn't be
   compiled into submissions? *)
signature POWER_UTIL =
sig

  (* All inputs should be in lowercase! StringUtil.lcase! *)

  val loadproblem : int -> Board.problem
  val problems : unit -> Board.problem vector

  val get_valid_prefix : Board.problem -> Word32.word -> string -> int

  (* Given all the problems to try, and a solution, return the
     (prefix size, problem index, seed) for which the prefix can be
     longest while still being valid. *)
  val longest_prefix : Board.problem vector -> string ->
    int * int * Word32.word


  val is_excluded : string -> bool
  val is_known : string -> bool
  val is_invalid : string -> bool

end