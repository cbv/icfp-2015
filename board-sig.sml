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
  type piece

  (* val read_problem : json -> problem *)

  (* mutable version *)
  type state

  (* legal command characters *)
  eqtype legalchar

  (* Restart the problem, creating the initial state. It's
     fine to have multiple outstanding states for the same
     problem. *)
  val reset : problem -> state

  (* Make an exact copy of the state with a new identity *)
  val clone : state -> state

  (* Get an arbitrary character for the command,
     in case we aren't concerned with spelling
     phrases of power. *)
  val anychar : command -> legalchar
  (* Just check that the char is legal, returning it.
     Otherwise, raise an exception. *)
  val legalize : char -> legalchar
  val getchars : command -> legalchar vector
  val charcommand : legalchar -> command

  val move : state * legalchar -> moveresult

  (* Apply the move, updating the state, and then call the continuation
     with the result. When the continuation returns, undo the move
     (assuming it is the ONLY change to the state) and return whatever
     the continuation returned. *)
  val move_unwind : state * legalchar -> (moveresult -> 'a) -> 'a

  (* Also return a function that undoes the change to the state (assuming
     it is the ONLY change); this function should be called at most once. *)
  val move_undo : state * legalchar -> { result: moveresult,
                                         undo: unit -> unit }

  (* Human-readable for interactivity *)
  val toascii : state -> string

  (* Is the cell full/empty? Doesn't count the current active piece.
     Anything outside the valid bounds of the board counts as "full." *)
  val isfull : state * int * int -> bool
  val isempty : state * int * int -> bool

  (* Does this cell currently contain a member of the piece?
     This does not necessarily include the pivot. *)
  val ispiece : state * int * int -> bool
  (* Is this the pivot of the current piece? Is not necessarily
     a member of the piece. *)
  val ispivot : state * int * int -> bool
  val pivot : state -> int * int

  val size : problem -> int * int
  val width : problem -> int
  val height : problem -> int
  val pieces : problem -> piece vector

  (* TODO: functional versions *)
  (*
  val freeze : state -> fstate
  val thaw : fstate -> state
  *)

end
