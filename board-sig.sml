signature BOARD =
sig

  exception Board of string

  datatype dir = E | W | SE | SW
  datatype turn = CW | CCW
  datatype command = D of dir | T of turn
  datatype reason =
      (* Gracefully used all pieces *)
      COMPLETE
      (* Last move locked a piece, but there's no space
         to place the next one. *)
    | NO_SPACE
      (* Bad command? *)
    | ERROR
  datatype moveresult =
      Continue of { scored: int, lines: int, locked: bool }
    | Done of { reason: reason }


  val dirstring : dir -> string
  val dirorder : dir * dir -> order
  val turnstring : turn -> string
  val turnorder : turn * turn -> order
  val commandstring : command -> string
  val commandorder : command * command -> order
  val reasonstring : reason -> string
  val moveresultstring : moveresult -> string
  (* val reasonorder : reason -> order *)

  (* The problem is always functional. *)
  type problem

  (* always functional; this denotes a specific piece that
     can occur *)
  type member

  (* val read_problem : json -> problem *)

  (* mutable version *)
  type state

  (* legal command characters *)
  eqtype legalchar

  val members : problem -> member vector

  (* Restart the problem, creating the initial state. It's
     fine to have multiple outstanding states for the same
     problem. *)
  val reset : problem -> state

  (* Get an arbitrary character for the command,
     in case we aren't concerned with spelling
     phrases of power. *)
  val anychar : command -> legalchar
  (* Just check that the char is legal, returning it.
     Otherwise, raise an exception. *)
  val legalize : char -> legalchar
  val getchars : command -> legalchar vector
  val charcommand : legalchar -> command
  val move : problem * state * legalchar -> moveresult


  (* Human-readable for interactivity *)
  val toascii : problem * state -> string

  val isfull : state * int * int -> bool
  val isempty : state * int * int -> bool

  val size : problem -> int * int
  val width : problem -> int
  val height : problem -> int

  (* TODO: functional versions *)
  (*
  val freeze : state -> fstate
  val thaw : fstate -> state
  *)

end
