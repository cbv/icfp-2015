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

  (* Can we legally execute the string at this point?
     Undoes any modifications to state. *)
  val can_execute : Board.state -> string -> bool
  val execute : Board.state -> string -> unit

  val is_excluded : string -> bool
  val is_known : string -> bool
  val is_invalid : string -> bool

  (* Escape a solution string so that it can be placed in single-quotes
     on a bash command-line. (It actually breaks out of those quotes
     to emit single quote characters.) *)
  val escape : string -> string

end