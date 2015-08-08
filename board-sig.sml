signature BOARD =
sig

  exception Board of string

  datatype dir = E | W | SE | SW
  datatype turn = CW | CCW
  datatype command = D of dir | T of turn
  datatype why =
  (* Gracefully used all pieces *)
    COMPLETE
  (* Last move locked a piece, but there's no space
     to place the next one. *)
  | NO_SPACE
  datatype status =
    (* Game keeps going *)
    CONTINUE
  | GAMEOVER of why
  (* Bad command? *)
  | ERROR

  datatype moveresult =
    (* scored is the points scored as a result of this move;
       lines is the number of lines created;
       locked is whether the move caused the piece to be locked;
       status tells us whether the game ended, etc. *)
    M of { scored: int, lines: int, new_phrases: int,
           locked: (int * int * int) option,
           status: status }

  val dirstring : dir -> string
  val dirorder : dir * dir -> order
  val turnstring : turn -> string
  val turnorder : turn * turn -> order
  val commandstring : command -> string
  val commandorder : command * command -> order
  val statusstring : status -> string
  val moveresultstring : moveresult -> string
  (* val statusorderorder : status -> order *)

  (* The problem is always functional. *)
  type problem

  (* always functional; this denotes a specific piece that
     can occur *)
  type piece

  (* Give it a json string. Assumes phrases of power from phrases.sml. *)
  val fromjson : string -> problem
  val fromjsonwithpower : string * string vector -> problem

  (* Create a new problem which is like the given one, but with different
     phrases of power.  Useful for trying out different power phrase sets. *)
  val setPower : problem * string vector -> problem

  (* mutable version *)
  type state

  val piece_position : state -> int * int
  val piece_angle : state -> int
  (* Get the symmetry group of the piece. Only changes when the piece
     changes (after lock). The symmetry group is either 1, 2, 3, or 6.
     Angles can be considered equivalent modulo the symmetry group
     (they produce the same set of members). *)
  val piece_symmetry : state -> int

  (* legal command characters *)
  eqtype legalchar

  (* Restart the problem, creating the initial state. Takes the seed
     index to use. It's fine to have multiple outstanding states for
     the same problem and seed. *)
  val reset : problem * int -> state
  (* Start the problem with a specific seed, which need not be
     in the problem description. *)
  val resetwithseed : problem * Word32.word -> state

  (* Make an exact copy of the state with a new identity *)
  val clone : state -> state

  (* Get an arbitrary character for the command,
     in case we aren't concerned with spelling
     phrases of power. *)
  val anychar : command -> legalchar
  (* Just check that the char is legal, returning it.
     Otherwise, raise an exception. *)
  val legalize : char -> legalchar
  val forgetlegal : legalchar -> char
  val getchars : command -> legalchar vector
  val charcommand : legalchar -> command

  val move : state * legalchar -> moveresult

  (* Apply the move, updating the state, and then call the continuation
     with the result. When the continuation returns, undo the move
     (assuming it is the ONLY change to the state) and return whatever
     the continuation returned. *)
  val move_unwind : state * legalchar * (moveresult -> 'a) -> 'a

  (* Also return a function that undoes the change to the state (assuming
     it is the ONLY change); this function should be called at most once. *)
  val move_undo : state * legalchar -> { result: moveresult,
                                         undo: unit -> unit }

  (* Human-readable for interactivity *)
  val toascii : state -> string
  val powerinfostring : state -> string

  (* Is the cell full/empty? Doesn't count the current active piece.
     Anything outside the valid bounds of the board counts as "full." *)
  val isfull : state * int * int -> bool
  val isempty : state * int * int -> bool

  (* Give the number of pieces that are remaining to spawn after the
     current one; assumes the state is still in a Continue state. *)
  val piecesleft : state -> int

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
  val seeds : problem -> Word32.word vector

  (* TODO: functional versions *)
  (*
  val freeze : state -> fstate
  val thaw : fstate -> state
  *)

  val uniformize_coord : (int * int) -> (int * int)
  val deuniformize_coord : (int * int) -> (int * int)
end
