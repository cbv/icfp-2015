signature BOARD =
sig

  exception Board of string

  datatype move = E | W | SE | SW
  datatype turn = CW | CCW

  val movestring : move -> string
  val moveorder : move * move -> order
  val turnstring : turn -> string
  val turnorder : turn * turn -> order

  (* The problem is always functional. *)
  type problem

  (* always functional; this denotes a specific piece that
     can occur *)
  type member

  (* val read_problem : json -> problem *)

  (* mutable version *)
  type state

  val members : problem -> member vector

  (* Restart the problem, creating the initial state. It's
     fine to have multiple outstanding states for the same
     problem. *)
  val reset : problem -> state

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
